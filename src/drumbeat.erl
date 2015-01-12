-module(drumbeat).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> 
    inets:start(),
    application:start(?MODULE).

start(_StartType, _StartArgs) ->
    drumbeat_sup:start_link().

stop(_State) ->
    ok.
