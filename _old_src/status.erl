% Module for parsing HTTP Statues
-module(status).

-export([is_success/2]).


classify(X) when X > 199, X < 300 -> success;
classify(X) when X > 299, X < 400 -> redirection;
classify(X) when X > 399, X < 500 -> client_error;
classify(X) when X > 499, X < 600 -> server_error.

is_success(Status, Accepted) when is_list(Accepted) ->
    lists:member(Status, Accepted);
is_success(Status, Accepted) when is_atom(Accepted) ->
    classify(Status) == Accepted.
    
