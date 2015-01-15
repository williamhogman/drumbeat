-module(sender).

-export([start/2, init/2]).

start(Dispatch, URL) ->
    spawn(sender, init, [Dispatch, URL]).

send_request(URL) ->
    httpc:request(URL).

init(Dispatch, URL) ->
    Res = case send_request(URL) of
	{ok, {{_Version, Status, _Reason}, Headers, Body}} -> {Status, Headers, Body}
    end,
    gen_server:cast(Dispatch, {response, Res}).
