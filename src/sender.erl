-module(sender).

-export([start/1, init/1]).

start(URL) ->
    spawn(sender, init, [URL]).

init(URL) ->
    io:format("Hello! ~s~n", [URL]),
    Req = httpc:request(URL),
    io:format("Boom! ~w~n", [Req]).
