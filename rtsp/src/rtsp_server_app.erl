-module(rtsp_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rtsp_server_sup:start_link().

stop(_State) ->
    ok.
