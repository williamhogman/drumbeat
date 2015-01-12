-module(sender_sup).
-export([start/0, init/1, start_worker/2]).
-behaviour(supervisor).
 
start() ->
    supervisor:start_link(?MODULE, []).
 
init([]) ->
    MaxRestart = 5,
    MaxTime = 3600,
    {ok, {{simple_one_for_one, MaxRestart, MaxTime},
	  [{sender,
	    {sender, start, []},
	    temporary, 5000, worker, [sender]}]}}.

start_worker(SenderPid, URL) ->
    supervisor:start_child(SenderPid, [URL]).
    
