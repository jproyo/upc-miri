-module(opty).
-export([start/5, start/6, start/7, stop/1]).

%% This starts in local mode
%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
start(Clients, Entries, Reads, Writes, Time) ->
    start(Clients, Entries, Reads, Writes, Entries, Time).

%% This starts in local mode
%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
%% AmountEntriesAllowed: Amount of Entries Allowed for each client. 
start(Clients, Entries, Reads, Writes, AmountEntriesAllowed, Time) ->
    start(local, Clients, Entries, Reads, Writes, AmountEntriesAllowed, Time).

%% Mode: remote|local 
%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
%% AmountEntriesAllowed: Amount of Entries Allowed for each client. 
start(Mode, Clients, Entries, Reads, Writes, AmountEntriesAllowed, Time) ->
    startServer(Mode, Entries),
    L = startClients(Mode, Clients, [], Entries, Reads, Writes, AmountEntriesAllowed),
    io:format("Starting: ~w MODE ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, ~w ALLOWE, DURATION ~w s~n", 
              [Mode, Clients, Entries, Reads, Writes, AmountEntriesAllowed, Time]),
    timer:sleep(Time*1000),
    stop(L).

startServer(Mode, Entries) -> 
    if Mode == remote ->
            register(s, server:start(node(), Entries));
       true -> 
            register(s, server:start(Entries))
    end.

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    s ! stop,
    io:format("Stopped~n").

startClients(_, 0, L, _, _, _, _) -> L;
startClients(Mode, Clients, L, Entries, Reads, Writes, AmountEntriesAllowed) ->
    EntriesClientAllowed = lists:sublist([X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- lists:seq(1, Entries)])], AmountEntriesAllowed),
    if Mode == remote -> 
            Pid = client:start(client_node(), Clients, EntriesClientAllowed, Reads, Writes, {s, node()}),
            startClients(Mode, Clients-1, [Pid|L], Entries, Reads, Writes, AmountEntriesAllowed);
       true ->
            Pid = client:start(Clients, EntriesClientAllowed, Reads, Writes, s),
            startClients(Mode, Clients-1, [Pid|L], Entries, Reads, Writes, AmountEntriesAllowed)
    end.
 

stopClients([]) ->
    ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},	
    stopClients(L).

waitClients([]) ->
    ok;
waitClients(L) ->
    receive
        {done, Pid} ->
            waitClients(lists:delete(Pid, L))
    end.


% Calculate Clients Node name. 
client_node() ->
    {_, Hostname} = inet:gethostname(),
    AccNode = "opty-client@"++Hostname,
    list_to_atom(AccNode).


