-module(proposer).
-export([start/6,start/7]).
-import(order,[first/1,null/0]).
-define(timeout, 2000).
-define(backoff, 10).
-define(sorriesOptimizationON,1).

start(Name, Proposal, Acceptors, Sleep, PanelId, Config) ->
    spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Config) end).

start(Location,Name, Proposal, Acceptors, Sleep, PanelId, Config) ->
    spawn(Location,fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Config) end).


init(Name, Proposal, Acceptors, Sleep, PanelId, Config) ->
    timer:sleep(Sleep),
    Round = order:first(Name),
    round(Name, ?backoff, Round, Proposal, Acceptors, PanelId, Config).

round(Name, Backoff, Round, Proposal, Acceptors, PanelId, Config) ->
    
    io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n",
              [Name, Round, Proposal]),
                                                % Update gui
    PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
    case ballot(Name, Round, Proposal, Acceptors, PanelId, Config) of
        {ok, Decision} ->
            io:format("[Proposer ~w] DECIDED ~w in round ~w~n", [Name, Decision, Round]),
            PanelId ! stop,
            {ok, Decision};
        abort ->
            timer:sleep(rand:uniform(Backoff)),
            Next = order:inc(Round),
            round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId, Config)
    end.

voteResult(VoteResult,Value) ->
    case VoteResult of
        ok ->
            {ok, Value};
        abort ->
            abort
    end.

collectResult(CollectResult, Name, Round, Proposal, Acceptors, PanelId,Quorum, Config) ->          
    case CollectResult of
        {accepted, Value} ->
            io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n",
                      [Name, Round, Value, Proposal]),
                                                % update gui
            PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
            accept(Round, Value, Acceptors),
            case Config of
                {true} ->
                    VoteResult = vote(Quorum, Round, Quorum),
                    voteResult(VoteResult,Value);
                _ -> 
                io:format("[Proposer ~w] Config: ~w~n",
              [Name, Config]),
                    VoteResult = vote(Quorum, Round),
                    voteResult(VoteResult,Value)
            end;           
        abort ->
            abort
    end.

ballot(Name, Round, Proposal, Acceptors, PanelId, Config) ->
    prepare(Round, Acceptors),
    Quorum = (length(Acceptors) div 2) + 1,
    MaxVoted = order:null(),
    case Config of
        {true} ->
            CollectResult = collect(Quorum, Round, MaxVoted, Proposal, Quorum),
            collectResult(CollectResult, Name, Round, Proposal, Acceptors, PanelId,Quorum, Config);
        _ -> 
            CollectResult = collect(Quorum, Round, MaxVoted, Proposal),
            collectResult(CollectResult, Name, Round, Proposal, Acceptors, PanelId,Quorum, Config)
        end.

collect(0, _, _, Proposal) ->
    {accepted, Proposal};
collect(N, Round, MaxVoted, Proposal) ->
    receive
        {promise, Round, _, na} ->
            collect(N-1, Round, MaxVoted, Proposal);
        {promise, Round, Voted, Value} ->
            case order:gr(Voted, MaxVoted) of
                true ->
                    collect(N-1, Round, Voted, Value);
                false ->
                    collect(N-1, Round, MaxVoted, Proposal)
            end;
        {promise, _, _, _} ->
            collect(N, Round, MaxVoted, Proposal);       
        {sorry, _} ->
            collect(N, Round, MaxVoted, Proposal)            
    after ?timeout ->
        io:format("[Proposer] Aborted for timeout without sorries~n"),
            abort
    end.

collect(0, _, _, Proposal,_) ->
    {accepted, Proposal};
collect(_, _, _, _,0) ->
    io:format("[Proposer] Aborted collect for sorries received~n"),
    abort;
collect(N, Round, MaxVoted, Proposal,NotSorries) ->
    receive
        {promise, Round, _, na} ->
            collect(N-1, Round, MaxVoted, Proposal,NotSorries);
        {promise, Round, Voted, Value} ->
            case order:gr(Voted, MaxVoted) of
                true ->
                    collect(N-1, Round, Voted, Value,NotSorries);
                false ->
                    collect(N-1, Round, MaxVoted, Proposal,NotSorries)
            end;
        {promise, _, _, _} ->
            collect(N, Round, MaxVoted, Proposal,NotSorries);       
        
        {sorry, {prepare, Round}} ->
            io:format("[Proposer] sorry less~n"),
            
            collect(N, Round, MaxVoted, Proposal,NotSorries-1);
        {sorry, _} ->
            collect(N, Round, MaxVoted, Proposal,NotSorries)            
    after ?timeout ->
        io:format("[Proposer] Aborted for timeout~n"),
    
            abort
    end.

vote(0, _) ->
    ok;
vote(N, Round) ->
    receive
        {vote, Round} ->
            vote(N-1, Round);
        {vote, _} ->
            vote(N, Round);
        {sorry, {accept, Round}} ->
            vote(N, Round);
        {sorry, _} ->
            vote(N, Round)
    after ?timeout ->
            abort
    end.

vote(0,_,_) ->
    ok;
vote(_,_,0) ->
    abort;
vote(N, Round,NotSorries) ->
    receive
        {vote, Round} ->
            vote(N-1, Round,NotSorries);
        {vote, _} ->
            vote(N, Round,NotSorries);
        {sorry, {accept, Round}} ->
            vote(N, Round,NotSorries-1);
        {sorry, _} ->
            vote(N, Round,NotSorries)
    after ?timeout ->
            abort
    end.


prepare(Round, Acceptors) ->
    Fun = fun(Acceptor) ->
                  send(Acceptor, {prepare, self(), Round})
          end,
    lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
    Fun = fun(Acceptor) ->
                  send(Acceptor, {accept, self(), Round, Proposal})
          end,
    lists:foreach(Fun, Acceptors).

send(Name, Message) ->
    if is_tuple(Name) -> %remote
        Name ! Message;
    true -> %local
        case whereis(Name) of
            undefined ->
                down;
            Pid ->
                Pid ! Message
        end
    end.
