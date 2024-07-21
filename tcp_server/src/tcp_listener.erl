% TCP Listener Process (tcp_listener.erl)
-module(tcp_listener).
-author('saleyn@gmail.com').

%% 实现 gen_server 模式
-behaviour(gen_server).

%% 内部接口
-export([start_link/2]).

%% gen_server 回调函数
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% 定义了一个 record 记录 gen_server 进程的状态
-record(state, {
                listener,       % Listening socket
                acceptor,       % Asynchronous acceptor's internal reference
                module          % FSM handling module
               }).

%%--------------------------------------------------------------------
%% @spec (Port::integer(), Module) -> {ok, Pid} | {error, Reason}
%% @doc 监控树调用并开始进行tcp套接字监听
%% @end
%%----------------------------------------------------------------------
start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc gen_server启动时回调，并创建 tcp 监听
%% @end
%%----------------------------------------------------------------------
init([Port, Module]) ->
    process_flag(trap_exit, true),
    Opts = [binary, {packet, 2}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}],
    %%　使用 gen_tcp 模块启动套接字监听，这是一个阻塞动作
    case gen_tcp:listen(Port, Opts) of
    {ok, Listen_socket} -> %% 创建监听成功返回监听socket
        %% 创建第一个接受连接的进程
        %% prim_inet:async_accept/2开启异步监听
        %% 之后有客户端连接时会向此进程发送一个异步消息inet_async到进程消息队列
        %% Ref 存储接受进程的引用
        {ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
        {ok, #state{listener = Listen_socket,
                    acceptor = Ref,
                    module   = Module}};
    {error, Reason} ->
        {stop, Reason}
    end.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc 服务进程被同步调用时的回调函数
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc 服务进程被异步调用时的回调函数
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc 回调函数，处理那些直接发消息到进程邮箱的事件
%% 这里处理的是 {inet_async, ListSock, Ref, {ok, CliSocket}}事件，
%% inet_async 表示是一个异步事件，服务器端接收连接采用异步的方式，
%% 客户端连接最终会被转化成一个 inet_async 消息发送到进程邮箱等待处理
%% {{ok, CliSocket}} 里的CliSocket表示客户端建立的连接套接口
%% @end
%% @private
%%-------------------------------------------------------------------------

%% 注意这里 ListSock 以及 Ref 做了匹配,只有匹配了才是该监听口接收的连接
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state{listener=ListSock, acceptor=Ref, module=Module} = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
        ok              -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
        end,

        %% 接收新的客户端连接，启动一个新的客户端状态机进程，动态添加到 tcp_client_sup 客户端监控树
        {ok, Pid} = tcp_server_app:start_client(),

        %% 绑定 CliSocet 到客户端进程 Pid, 这样CliSocket接收数据都会被转化成Pid代表进程的邮箱消息
        gen_tcp:controlling_process(CliSocket, Pid),
        %% Instruct the new FSM that it owns the socket.

        Module:set_socket(Pid, CliSocket),

        %% Signal the network driver that we are ready to accept another connection
        %% 重新设置异步监听下一个客户端连接的消息，设置新的监听引用
        %% 必须重新设置才能监听到 {inet_async,S,Ref,Status} 消息
        case prim_inet:async_accept(ListSock, -1) of
        {ok,    NewRef} -> ok;
        {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

         %% 更新新的监听引用
        {noreply, State#state{acceptor=NewRef}}
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
        {stop, Why, State}
    end;

%%客户端建立连接的容错处理
handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%% 设置客户端socket的参数选项，只是简单的复制了监听服务器的配置选项
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.
