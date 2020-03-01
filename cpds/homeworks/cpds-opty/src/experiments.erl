-module(experiments).
-export([ change_clients/1
        , change_entries/1
        , change_reads/1
        , change_writes/1
        , change_ratio_read_writes/1
        , change_accessed_entries/1
        , change_clients/0
        , change_entries/0
        , change_reads/0
        , change_writes/0
        , change_ratio_read_writes/0
        , change_accessed_entries/0]).
-import(opty,[start/7]).
-define(NClients,3).
-define(NEntries,5).
-define(NReads,1).
-define(NWrites,1).
-define(AccessedEntries,5).
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
    change_clients(?DefMode).

change_entries() ->
    change_entries(?DefMode).

change_reads() ->
    change_reads(?DefMode).

change_writes() ->
    change_writes(?DefMode).

change_ratio_read_writes() ->
    change_ratio_read_writes(?DefMode).

change_accessed_entries() ->
    change_accessed_entries(?DefMode).

change_clients(Mode) ->
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites, ?NEntries,?Time),
    opty:start(Mode,?NClients+5,?NEntries, ?NReads, ?NWrites, ?NEntries, ?Time),
    opty:start(Mode,?NClients+10,?NEntries, ?NReads, ?NWrites, ?NEntries, ?Time),
    opty:start(Mode,?NClients+15,?NEntries, ?NReads, ?NWrites, ?NEntries, ?Time),
    exit(0).

change_entries(Mode) ->
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites, ?NEntries, ?Time),
    opty:start(Mode,?NClients,?NEntries+5, ?NReads, ?NWrites, ?NEntries+5, ?Time),
    opty:start(Mode,?NClients,?NEntries+10, ?NReads, ?NWrites, ?NEntries+10,?Time),
    opty:start(Mode,?NClients,?NEntries+15, ?NReads, ?NWrites, ?NEntries+15, ?Time),
    exit(0).

change_reads(Mode) ->
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites, ?NEntries, ?Time),
    opty:start(Mode,?NClients,?NEntries, ?NReads+5, ?NWrites, ?NEntries, ?Time),
    opty:start(Mode,?NClients,?NEntries, ?NReads+10, ?NWrites, ?NEntries, ?Time),
    opty:start(Mode,?NClients,?NEntries, ?NReads+15, ?NWrites, ?NEntries, ?Time),
    exit(0).

change_writes(Mode) ->
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites, ?NEntries, ?Time),
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites+5, ?NEntries, ?Time),
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites+10, ?NEntries, ?Time),
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites+15, ?NEntries, ?Time),
   exit(0).

change_ratio_read_writes(Mode) ->
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites, ?NEntries, ?Time),
    opty:start(Mode,?NClients,?NEntries,0,10,?NEntries,3),
    opty:start(Mode,?NClients,?NEntries,1,9,?NEntries,3),
    opty:start(Mode,?NClients,?NEntries,2,8,?NEntries,3),
    opty:start(Mode,?NClients,?NEntries,4,6,?NEntries,3),
    opty:start(Mode,?NClients,?NEntries,5,5,?NEntries,3),
    opty:start(Mode,?NClients,?NEntries,6,4,?NEntries,3),
    opty:start(Mode,?NClients,?NEntries,8,2,?NEntries,3),
    opty:start(Mode,?NClients,?NEntries,9,1,?NEntries,3),
    opty:start(Mode,?NClients,?NEntries,10,0,?NEntries,3),
    exit(0).


change_accessed_entries(Mode) ->
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites, ?AccessedEntries, ?Time),
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites, ?AccessedEntries-1, ?Time),
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites, ?AccessedEntries-2, ?Time),
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites, ?AccessedEntries-3, ?Time),
    opty:start(Mode,?NClients,?NEntries, ?NReads, ?NWrites, ?AccessedEntries-4, ?Time),

    opty:start(Mode,?NClients,?NEntries+10, ?NReads, ?NWrites+14, ?AccessedEntries+10, ?Time),
    opty:start(Mode,?NClients,?NEntries+10, ?NReads, ?NWrites+14, ?AccessedEntries+7, ?Time),
    opty:start(Mode,?NClients,?NEntries+10, ?NReads, ?NWrites+14, ?AccessedEntries+5, ?Time),
    opty:start(Mode,?NClients,?NEntries+10, ?NReads, ?NWrites+14, ?AccessedEntries+3, ?Time),
    opty:start(Mode,?NClients,?NEntries+10, ?NReads, ?NWrites+14, ?AccessedEntries+2, ?Time),
    opty:start(Mode,?NClients,?NEntries+10, ?NReads, ?NWrites+14, ?AccessedEntries+1, ?Time),

    exit(0).
