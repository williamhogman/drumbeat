-module(drumbeat_dispatch_serv).
-behaviour(gen_server).
-export([start/1, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {dispatch, sender}).

start(Pid) ->
    gen_server:start(?MODULE, Pid, []).

init(Pid) ->
    gen_server:cast(self(), start_worker_sup),
    {ok, #state{dispatch=Pid}}.

handle_call(_Request, _From, State) ->
    {reply, ack, State}.

handle_cast(start_worker_sup, #state{dispatch=Dispatch} = State) ->
    Sender = drumbeat_dispatch_sup:start_sender_sup(Dispatch),
    add_request(self(), "http://pg.whn.se"),
    {noreply, State#state{sender=Sender}};
handle_cast({request, URL}, State) ->
    sender_sup:start_worker(State#state.sender, URL),
    {noreply, State};
handle_cast({response, {Status, Headers, Body}}, State) ->
    io:format("Status:~3..0B~n Headers: ~p~n Body: ~s~n", [Status, Headers, Body]),
    {noreply, State}.


handle_info(_, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok;
terminate(Err, _) ->
    Err.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% API
add_request(Pid, URL) ->
    gen_server:cast(Pid, {request, URL}).
