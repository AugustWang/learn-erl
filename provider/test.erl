-module(test).


%%
%% Exported Functions
%%
%% -compile(export_all).

-include_lib("kernel/include/file.hrl").
-include("hdlt_logger.hrl").
-include("package.hrl").
-include("globalDefine.hrl").
-include("db.hrl").
-include("mysql.hrl").
-include("chat.hrl").
-include("top.hrl").

-export([
        reload/0
        ,reload/1
        ,task_test/0
        ,task_test2/0
        ,file_name/0
        ,room_test/0
        ,timestamp/1
        ,sendSysMsgToAllPlayer/2
        ,sendSysMsgToAllPlayer2/2
        ,rank_top_login_notice/0
        ,test1/0
        ,test2/0
    ]).

-export(
    [
        execute/2
        ,get_one/2
        ,get_row/2
        ,get_all/2
        ,db_test/0
    ]
).

%% @doc 加载ebin下所有的文件
reload() ->
    FileName = file_name(),
    reload1(FileName).

%% @doc 加载Time分钟内修改的文件
reload(Time) when is_integer(Time) ->
    FileName = file_name(Time * 60),
    reload1(FileName);

%% @doc 加载Module文件
reload(Module) ->
    io:format("Reloading ~p ...", [Module]),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            io:format(" ok.~n");
            %% case erlang:function_exported(Module, test, 0) of
            %%     true ->
            %%         io:format(" - Calling ~p:test() ...", [Module]),
            %%         case catch Module:test() of
            %%             ok ->
            %%                 io:format(" ok.~n"),
            %%                 reload;
            %%             Reason ->
            %%                 io:format(" fail: ~p.~n", [Reason]),
            %%                 reload_but_test_failed
            %%         end;
            %%     false ->
            %%         reload
            %% end;
        {error, Reason} ->
            io:format(" fail: ~p.~n", [Reason]),
            error
    end.

reload1([]) -> ok;
reload1([Mod|T]) ->
    Module = case is_list(Mod) of
        true -> list_to_atom(Mod);
        false -> Mod
    end,
    reload(Module),
    reload1(T).

file_name() ->
    case file:get_cwd() of
        {ok, _Dir} ->
            Dir = "E:/work/xiaohua_server/gameserver/ebin",
            case file:list_dir(Dir) of
                {ok, Filenames} ->
                    %% io:format("Filenames:~w", [Filenames]),
                    file_name1(Filenames, []);
                {error, Reason} ->
                    io:format("file list_dir error:~w", [Reason]),
                    []
            end;
        {error, Reason} ->
            io:format("file get_cwd error:~w", [Reason]),
            []
    end.

file_name1([], R) -> R;
file_name1([Name|List], R) ->
    case string:str(Name,"beam") =/= 0 of
        true ->
            [H|_] = string:tokens(Name, "."),
            file_name1(List, [H|R]);
        false ->
            file_name1(List, R)
    end.

file_name(Time) ->
    case file:get_cwd() of
        {ok, _Dir} ->
            Dir = "E:/work/xiaohua_server/gameserver/ebin",
            case file:list_dir(Dir) of
                {ok, Filenames} ->
                    file_name2(Filenames, Time, []);
                {error, Reason} ->
                    io:format("file list_dir error:~w", [Reason]),
                    []
            end;
        {error, Reason} ->
            io:format("file get_cwd error:~w", [Reason]),
            []
    end.

file_name2([], _Time, R) -> R;
file_name2([Name|List], Time, R) ->
    case string:str(Name,"beam") =/= 0 of
        true ->
            %% file:set_cwd("../ebin"),
            case file:read_file_info("../ebin/"++Name, [{time, posix}]) of
                {ok, FileInfo} ->
                    Now = timestamp(now),
                    %% io:format("FileInfo record:~w", [FileInfo#file_info.mtime]),
                    case Now - FileInfo#file_info.mtime < Time of
                        true ->
                            [H|_] = string:tokens(Name, "."),
                            file_name2(List, Time, [H|R]);
                        false ->
                            file_name2(List, Time, R)
                    end;
                {error, Reason} ->
                    io:format("file read_file_info (~w) error Reason:~w", [Name,Reason]),
                    []
            end;
        false ->
            file_name2(List, Time, R)
    end.

task_test() ->
    case db:openBinData("taskReward.bin") of
        [] -> ok;
        Data ->
            db:loadBinData(Data, task_test),
            Datalist = db:matchObject(task_test, #task_reward{ _='_' } ),
            ?DEBUG_OUT("...~w", [Datalist])
    end.

task_test2() ->
    case file:read_file_info("taskReward.bin", [{time, local}]) of
        {ok, FileInfo} ->
            io:format("FileInfo record:~w", [FileInfo#file_info.mtime]),
            FileInfo;
        {error, Reason} ->
            io:format("Reason:~w", [Reason])
    end.

room_test() ->
    case db:openBinData("studio_exp.bin") of
        [] -> ok;
        Data ->
            db:loadBinData(Data, room_test),
            Datalist = db:matchObject(room_test, #trainRoomCostCfg{ _='_' } ),
            ?DEBUG_OUT("...~w", [Datalist])
    end.

timestamp(now) ->
    {M, S, _MC} = erlang:now(),
    M * 1000000 + S;

timestamp(seconds) ->
    {M, S, MC} = erlang:now(),
    M * 1000000 + S + MC / 1000000;

timestamp(today) ->
    {M, S, MC} = erlang:now(),
    {_, Time} = calendar:now_to_local_time({M,S,MC}),
    M * 1000000 + S - calendar:time_to_seconds(Time);

timestamp(tomorrow) ->
    timestamp(today) + 86400;

timestamp(yesterday) ->
    timestamp(today) - 86400.

sendSysMsgToAllPlayer(world, Text ) ->
    ets:foldl(fun sendSysMsgToAllPlayer1/2, Text, ?SocketDataTableAtom).

sendSysMsgToAllPlayer1(#socketData{socket = Socket}, Text) ->
    message:send(Socket, #pk_SysMessage{type = ?SYSTEM_MESSAGE_ANNOUNCE, text = Text}).

sendSysMsgToAllPlayer2(world, Text ) ->
    ets:foldl(fun sendSysMsgToAllPlayer3/2, Text, ?PlayerGlobalTableAtom).

sendSysMsgToAllPlayer3(#playerGlobal{pid = Pid}, Text) ->
    player:sendToPlayerProcByPID(Pid, #pk_SysMessage{type = ?SYSTEM_MESSAGE_ANNOUNCE, text = Text}).

%% @doc 执行一条sql,返回影响的数据条数
-spec execute(Pool, Sql) -> {ok, Num} | {error, Reason} when
    Pool :: atom(),
    Sql :: binary(),
    Num :: integer(),
    Reason :: term().
execute(Pool, Sql) ->
    %% mysql:fetch(Pool, Sql).
    case mysql:fetch(Pool, Sql) of
        {updated, {_,_,_,Num,_,_,_,_}} -> {ok, Num};
        {error, {_,_,_,_,_,Reason,_,_}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 执行一条sql,返回第一行
-spec get_one(Pool, Sql) -> {ok, Data} | {error, Reason} when
    Pool :: atom(),
    Sql :: binary(),
    Data :: term(),
    Reason :: term().
get_one(Pool, Sql) ->
    %% mysql:fetch(Pool, Sql).
    case mysql:fetch(Pool, Sql) of
        {data, {_,_,[],_,_,_,_,_}} -> {error, null};
        {data, {_,_,[[Data]],_,_,_,_,_}} -> {ok, Data};
        {error, {_,_,_,_,_,Reason,_,_}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 执行一条sql,返回第一行第一列
-spec get_row(Pool, Sql) -> {ok, Data} | {error, Reason} when
    Pool :: atom(),
    Sql :: binary(),
    Data :: term(),
    Reason :: term().
get_row(Pool, Sql) ->
    %% mysql:fetch(Pool, Sql).
    case mysql:fetch(Pool, Sql) of
        {data, {_,_,[],_,_,_,_,_}} -> {error, null};
        {data, {_,_,[Data],_,_,_,_,_}} -> {ok, Data};
        {error, {_,_,_,_,_,Reason,_,_}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 执行一条sql,返回所有行
-spec get_all(Pool, Sql) -> {ok, Data} | {error, Reason} when
    Pool :: atom(),
    Sql :: binary(),
    Data :: term(),
    Reason :: term().
get_all(Pool, Sql) ->
    %% mysql:fetch(Pool, Sql).
    case mysql:fetch(Pool, Sql) of
        {data, {_,_,[],_,_,_,_,_}} -> {error, null};
        {data, {_,_,Data,_,_,_,_,_}} -> {ok, Data};
        {error, {_,_,_,_,_,Reason,_,_}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

db_test() ->
    %% Sql = list_to_binary([<<"insert into log_test(`gold`, `diamond`) values ( 100, 10000 )">>]),
    %% Sql1 = list_to_binary([<<"update log_test set `gold`=300 where `diamond` = 10000">>]),
    Sql2 = list_to_binary([<<"select `gold`, `diamond` from log_test ">>]),
    Sql3 = list_to_binary([<<"select count(*) from log_test ">>]),
    Sql4 = list_to_binary([<<"select count(*) from log_test where id = 10 ">>]),
    Sql5 = list_to_binary([<<"select `gold`, `diamond` from log_test where id = 11 ">>]),
    R2 = get_all(?LOGIN_LOG_DB_CONNECT_POOL, Sql2),
    R3 = get_all(?LOGIN_LOG_DB_CONNECT_POOL, Sql3),
    R4 = get_all(?LOGIN_LOG_DB_CONNECT_POOL, Sql4),
    R5 = get_all(?LOGIN_LOG_DB_CONNECT_POOL, Sql5),
    %% ?INFO("db_test 22 ~w", [R2]),
    %% ?INFO("db_test 33 ~w", [R3]),
    %% ?INFO("db_test 44 ~w", [R4]),
    %% ?INFO("db_test 55 ~w", [R5]).
    ?DEBUG_OUT("db_test 22 ~w", [R2]),
    ?DEBUG_OUT("db_test 33 ~w", [R3]),
    ?DEBUG_OUT("db_test 44 ~w", [R4]),
    ?DEBUG_OUT("db_test 55 ~w", [R5]).



test1() ->
    ets:match_object('worldBossMapInfo', '_').

test2() ->
    ets:match_object('itemCfgTableAtom', '_').

rank_top_login_notice() ->
    %% level rank
    top:getTopList(?TOP_TYPE_PLAYER_LEVEL),
    %% money rank
    top:getTopList(?TOP_TYPE_PLAYER_MONEY),
    %% capacity rank
    top:getTopList(?TOP_TYPE_PLAYER_FIGHTING_CAPACITY).


%%% vim: set filetype=erlang foldmarker=%%',%%. foldmethod=marker:
