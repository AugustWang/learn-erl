%% ---------------------------------------------------------------------
%% File: pingpong.erl

-module(pingpong).

-export([run/0]).

run() ->
    Pid = spawn(fun ping/0),
    Pid ! self(),
    receive
        %% pong -> ok
        pong -> error
    end.

ping() ->
    receive
        From -> From ! pong
    end.
