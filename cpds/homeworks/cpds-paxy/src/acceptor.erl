-module(acceptor).
-export([start/2,start/3,start/4]).
-import(order,[gr/2,goe/2,null/0]).
-import(pers,[open/1, read/1, store/5, close/1, delete/1]).
-define(DELAY, 200).
-define(DROP, na).

% Start Default function in Local mode
start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId, {?DELAY, ?DROP}) end).

% Start function with Location to start in Remote Mode and Configuration Specific for Acceptor
% Config =  {Delay,Drop} 
% Delay = Number in milliseconds to wait before sending a message to a proposer
% Drop = Percentage Number of Message to be Drop. Number is express in (Percentage/100)*10 for example 20 percent is 2
start(Location, Name, PanelId, Config) ->
    spawn(Location, fun() -> init(Name, PanelId, Config) end).

% Start in Local mode with configuration 
start(Name, PanelId, Config) ->
    spawn(fun() -> init(Name, PanelId, Config) end).

% Init Callback Spawn 
init(Name, PanelId, Config) ->
    {SPromised,SVoted,SValue,SPanelId} = initialState(Name),
    if
        SPanelId == na -> 
            acceptor(Name, SPromised, SVoted, SValue, PanelId, Config);
        true -> 
            acceptor(Name,SPromised,SVoted,SValue,SPanelId, Config),
            io:format("[Acceptor ~w] RECOVERING CRASH: [Promised: ~w,Voted: ~w,Value: ~w,Panel: ~w]~n",
                    [Name,SPromised,SVoted,SValue,SPanelId])
    end.

acceptor(Name, Promised, Voted, Value, PanelId, Config) ->
    {Delay, Drop} = Config,
    receive
        {prepare, Proposer, Round} ->
            Delay == na orelse timer:sleep(rand:uniform(Delay)),
            case order:gr(Round, Promised) of
                true ->
                    sendMessage(Drop, Proposer,{promise, Round, Voted, Value}),
                    io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                              [Name, Round, Voted, Value]),
                    % Update gui
                    Colour = case Value of na -> {0,0,0}; _ -> Value end,
                    PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                               "Promised: " ++ io_lib:format("~p", [Round]), Colour},
                    updateState(Name, Round, Voted, Value, PanelId), 
                    acceptor(Name, Round, Voted, Value, PanelId, Config);
                false ->
                    Proposer ! {sorry, {prepare, Round}},
                    acceptor(Name, Promised, Voted, Value, PanelId, Config)
            end;
        {accept, Proposer, Round, Proposal} ->
            Delay == na orelse timer:sleep(rand:uniform(Delay)),
            case order:goe(Round, Promised) of
                true ->
                    sendMessage(Drop, Proposer,{vote, Round}),
                    case order:goe(Round, Voted) of
                        true ->
                            io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                                        [Name, Promised, Round, Proposal]),
                            % Update gui
                            PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                                       "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
                            updateState(Name, Promised, Round, Proposal, PanelId), 
                            acceptor(Name, Promised, Round, Proposal, PanelId, Config);
                        false ->
                            acceptor(Name, Promised, Voted, Value, PanelId, Config)
                    end;
                false ->
                    Proposer ! {sorry, {accept, Round}},
                    acceptor(Name, Promised, Voted, Value, PanelId, Config)
            end;
        stop ->
            deleteState(Name),
            io:format("[Acceptor ~w] STOP SIGNAL ~n", [Name]),
            PanelId ! stop,
            ok
   end.

initialState(Name) ->
    FileName = Name,
    pers:open(FileName),
    Result = pers:read(FileName),
    pers:close(FileName),
    Result.

updateState(Name,Pr,Vt,Ac,Pn) ->
    FileName = Name,
    pers:open(FileName),
    pers:store(FileName,Pr,Vt,Ac,Pn),
    pers:close(FileName),
    true.

deleteState(Name) ->
    pers:delete(Name).

sendMessage(Drop,Pdest,Msg) ->
    if 
        Drop == na -> Pdest ! Msg;
        true ->
            P = rand:uniform(10),
            if P =< Drop ->
                    io:format("message dropped~n");
               true ->
                    Pdest ! Msg
            end
    end.
