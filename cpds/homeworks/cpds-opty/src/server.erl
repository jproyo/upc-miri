-module(server).
-export([start/1, start/2]).

start(N) ->
    spawn(fun() -> init(N) end).

start(Location, N) ->
    spawn(Location, fun() -> init(N) end).

init(N) ->
    Store = store:new(N),
    Validator = validator:start(),
    server(Validator, Store).
    
server(Validator, Store) ->
    receive 
        {open, Client} ->
            Client ! {transaction, Validator, Store},
            server(Validator, Store);
        stop ->
            Validator ! stop,
            store:stop(Store)
    end.
