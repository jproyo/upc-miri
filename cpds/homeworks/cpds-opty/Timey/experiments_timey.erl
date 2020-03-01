-module(experiments_timey).
-export([ change_clients/0
        , change_entries/0
        , change_reads/0
        , change_writes/0
        , change_ratio_read_writes/0]).
-import(timey,[start/5]).
-define(NClients,3).
-define(NEntries,5).
-define(NReads,1).
-define(NWrites,1).
-define(Time,3).
-define(DefMode,local).

%% Expriments can be run remote or local
%% To run remote just initiate an Erlang client instance with the following command:
%% `erl -sname opty-client -noshell`
%% To start the Server run the following command:
%% `erl -sname opty-srv -remsh opty-client`
%% Inside Erlang shell run the expriments as you wish
%% > experiments:change_clients(remote).

change_clients() ->
    timey:start(?NClients,?NEntries, ?NReads, ?NWrites, ?Time),
    io:format("test 2 ~n"),
    timey:start(?NClients+5,?NEntries, ?NReads, ?NWrites, ?Time),
    io:format("test 3 ~n"),
    timey:start(?NClients+10,?NEntries, ?NReads, ?NWrites, ?Time),
    io:format("test 4 ~n"),
    timey:start(?NClients+15,?NEntries, ?NReads, ?NWrites, ?Time),
    exit(0).

change_entries() ->
    timey:start(?NClients,?NEntries, ?NReads, ?NWrites, ?Time),
    timey:start(?NClients,?NEntries+5, ?NReads, ?NWrites, ?Time),
    timey:start(?NClients,?NEntries+10, ?NReads, ?NWrites, ?Time),
    timey:start(?NClients,?NEntries+15, ?NReads, ?NWrites, ?Time),
    exit(0).

change_reads() ->
    timey:start(?NClients,?NEntries, ?NReads, ?NWrites, ?Time),
    timey:start(?NClients,?NEntries, ?NReads+5, ?NWrites, ?Time),
    timey:start(?NClients,?NEntries, ?NReads+10, ?NWrites, ?Time),
    timey:start(?NClients,?NEntries, ?NReads+15, ?NWrites, ?Time),
    exit(0).

change_writes() ->
    timey:start(?NClients,?NEntries, ?NReads, ?NWrites, ?Time),
    timey:start(?NClients,?NEntries, ?NReads, ?NWrites+5, ?Time),
    timey:start(?NClients,?NEntries, ?NReads, ?NWrites+10, ?Time),
    timey:start(?NClients,?NEntries, ?NReads, ?NWrites+15, ?Time),
   exit(0).

change_ratio_read_writes() ->
    timey:start(?NClients,?NEntries,0,10, 3),
    timey:start(?NClients,?NEntries,1,9, 3),
    timey:start(?NClients,?NEntries,2,8, 3),
    timey:start(?NClients,?NEntries,4,6, 3),
    timey:start(?NClients,?NEntries,5,5, 3),
    timey:start(?NClients,?NEntries,6,4, 3),
    timey:start(?NClients,?NEntries,8,2, 3),
    timey:start(?NClients,?NEntries,9,1, 3),
    timey:start(?NClients,?NEntries,10,0, 3),
    exit(0).
