-module(drumbeat_dispatch_sup).
-behaviour(supervisor).

-export([start_link/0, start_sender_sup/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_sender_sup(SupPid) ->
    Spec = {
      sender_sup,
      {sender_sup, start, []},
      permanent, 5000, worker, [sender_sup]
     },
    {ok, SenderSup} = supervisor:start_child(SupPid, Spec),
    SenderSup.

init([]) ->
    PoolOpts = {one_for_all, 1, 3600},
    DispatchServ = {
      drumbeat_dispatch_serv, 
      {drumbeat_dispatch_serv, start, [self()]},
      permanent, 5000, worker, [drumbeat_dispatch_serv]
     },
    Children = [DispatchServ],
    Opts = {PoolOpts, Children},
    {ok, Opts}.
