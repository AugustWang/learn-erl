-module(my_bank).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).
-compile(export_all).
% -record(start,{}).

%% API

start_link() ->
	% 启动一个本地服务器
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
	% {ok, #state{}}.
	{ok, ets:new(?MODULE, [])}.

% new_account/添加账户; deposit/存款; withdraw/取款;
new_account(Who)           -> gen_server:call(?MODULE, {new,Who}).
deposit(Who, Amount)       -> gen_server:call(?MODULE, {add,Who,Amount}).
withdraw(Who, Amount)      -> gen_server:call(?MODULE, {remove,Who,Amount}).
query(Who)                 -> gen_server:call(?MODULE, {select,Who}).
stop()                     -> gen_server:call(?MODULE, stop).

% gen_server:call/2的第二个Request参数会作为handle_call/3的第一个参数
% From是发起调用的客户端进程Pid,
% State是当前的客户端状态

% handle_call(Request, From, State) ->
% 	Reply = ok, %% Reply作为远程过程调用的返回值发回给客户端
% 	{reply,Reply,State}.

handle_call({new, Who}, _From, State) ->
	% Reply = ok,
	Reply = case ets:lookup(State,Who) of
		[] -> ets:insert(State, {Who,0}),
			{welcome,Who};
		[_] -> {Who,you_already_are_a_customer}
	end,
	{reply, Reply, State};

handle_call({add, Who, Amount}, _From, State) ->
	Reply = case ets:lookup(State, Who) of
		[] -> not_a_customer;
		[{Who, X}] when Amount > 0 ->
			NewAmount = Amount + X,
			ets:insert(State, {Who, NewAmount}),
			{thanks, Who, your_balance_is, NewAmount};
		[{Who, _X}] ->
			{sorry,Who,you_Amount,Amount,less_than_zero}
	end,
	{reply, Reply, State};

handle_call({remove, Who, Amount}, _From, State) ->
	Reply = case ets:lookup(State,Who) of
		[] -> not_a_customer;
		[{Who, X}] when X >= Amount ->
			NewAmount = X - Amount,
			{thanks,Who, your_balance_is, NewAmount};
		[{Who, X}] ->
			{sorry,Who,you_only_have,X,in_the_bank}
	end,
	{reply, Reply, State};

handle_call({select, Who}, _From, State) ->
	Reply = case ets:lookup(State, Who) of
		[] -> not_a_customer;
		[{Who, X}] -> {hello, Who,you_bank_amount,X}
	end,
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
