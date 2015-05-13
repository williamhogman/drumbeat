-module(sender).

-export([start/2, init/2]).

-define(TIMEOUT, 10000).
-define(CONNECT_TIMEOUT, 5000).

start(Dispatch, URL) ->
    spawn_link(sender, init, [Dispatch, URL]).


send_request(URL, Opts) ->
    Method = proplists:get_value(method, Opts, get),
    Request = {URL, []},
    HttpOptions = [
		   {timeout, ?TIMEOUT},
		   {connect_timeout, ?CONNECT_TIMEOUT}
		  ],
    Options = [
	       {sync, false}
	      ],
    httpc:request(Method, Request, HttpOptions, Options).


% Just blow up for now
handle_error(Reason) -> io:format("~p~n", [Reason]), erlang:error(Reason).

simplify_response({{_Version, Status, _Reason}, Headers, Body}) -> {Status, Headers, Body}.


parse_status(Status, Opts) ->
    Accepted = proplists:get_value(ok_status, Opts, success),
    status:is_success(Status, Accepted).

handle_response({Status, Headers, Body}, Opts) ->
    case parse_status(Status, Opts) of
	true -> {done, Headers, Body};
	false -> retry
    end.


attempt_request(URL, Opts) ->
    {ok, ReqId} = send_request(URL, Opts),
    receive
	{http, {ReqId, {error, Reason}}} -> handle_error(Reason);
	{http, {ReqId, Result}} -> 
	     handle_response(simplify_response(Result), Opts)
    end.


get_retries(State) -> maps:get(retries,  State, 0).

increment_retries(State) ->
    Counter = 1 + get_retries(State),
    maps:put(retries, Counter, State).

init(Dispatch, URL) ->
    loop(Dispatch, URL, [], #{}).


loop(Dispatch, URL, Opts, State) ->
    case attempt_request(URL, Opts) of
	retry ->
	    NewState = increment_retries(State),
	    case get_retries(NewState) of
		X when X < 5 ->
		    io:format("~p~n", [NewState]),
		    loop(Dispatch, URL, Opts, NewState);
		X when X > 4 ->
		    gen_server:cast(Dispatch, {gaveup, retries})
	    end;
	{done, Headers, Body} -> drumbeat_dispatch_serv:request_received(Dispatch, Headers, Body)
	    
    end.