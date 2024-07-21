%%%----------------------------------------------------------------------
%%%
%%% wg @copyright 2009
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc the test module
%%%
%%%----------------------------------------------------------------------
-module(test).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg_crontab.hrl").

-compile([export_all]).

start() ->
    {ok, Pid} = wg_cron_server:start_link(),
    unlink(Pid),
    io:get_line("press Ctrl + C exit...").


run(N) ->
    io:format("run the routine by cron server:~p~n", [N]).
