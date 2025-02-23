-module(myapp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% rebar eunit test
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    myapp_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).
simple_test() ->
	ok = application:start(myapp),
	?assertNot(undefined == whereis(myapp_sup)).
-endif.
