-module(paxy).
-export([start/6,start/4,start/3,start/2,start/1,stop/0,crash/2,crash/1,stop/1]).
-import(lists,[map/2,seq/2,filter/2,nth/2]).
-import(inet,[gethostname/0]).
-import(string,[prefix/2]).
-define(DEF_NUM_ACC, 5).
-define(DEF_NUM_PROP, 3).
-define(DEF_CONFIG_ACC, {na,na}).
-define(DEF_CONFIG_PRO, {false}).
-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(YELLOW, {0,255,255}).
-define(PURPLE, {255,255,0}).


% Helper function to start Paxy with No extra configuration in Local mode
start(Sleep) ->
  start(local, ?DEF_CONFIG_ACC, ?DEF_CONFIG_PRO,Sleep, ?DEF_NUM_ACC, ?DEF_NUM_PROP).

% Helper function to start Paxy with No extra configuration but providing remote|local mode
start(Mode, Sleep) ->
  start(Mode, ?DEF_CONFIG_ACC, ?DEF_CONFIG_PRO,Sleep, ?DEF_NUM_ACC, ?DEF_NUM_PROP).

% Helper function to start Paxy with Configuration either for Acceptor or Proposers
start(Mode, ConfAccOrProp, Sleep) ->
    case ConfAccOrProp of
        {_, _} -> start(Mode, ConfAccOrProp, ?DEF_CONFIG_PRO, Sleep);
        _      -> start(Mode, ?DEF_CONFIG_ACC, ConfAccOrProp, Sleep)
    end.

% Helper function to start Paxy providing:
% - Default Acceptor Configuration
% - Default Proposer Configuration
% - remote|local mode
% - Specific number of acceptors and proposers
start(Mode, ConfigOrNumAcceptors, ConfigOrNumProposers,Sleep) ->
    case ConfigOrNumAcceptors of
      {_ , _} ->
          start(Mode, ConfigOrNumAcceptors, ConfigOrNumProposers,Sleep, ?DEF_NUM_ACC, ?DEF_NUM_PROP);
      true ->
        start(Mode, ?DEF_CONFIG_ACC, ?DEF_CONFIG_PRO,Sleep, ConfigOrNumAcceptors, ConfigOrNumProposers)
    end.

% Mode - could be remote|local
% ConfigAcc - Acceptor Special Configuration. Should be a Tupple with the form {Delay, Drop policy} of Acceptors. 
%             For example {1000,3} is going to Delay 1000 ms messages from acceptors to proposers and
%             is going to drop 30% of the messages to simulate failures.
% ConfigPro - Proposer Special Configuration. Should be a Tupple with the format:{SorriesOptimizationOn} where 
%             SorriesOptimizationOn is a boolean which activates/deactivates the sorries optimization
% Sleep - is a list with the initial sleep time for each proposer
% NumAcceptors - Set the amount of acceptors you want to start
% NumProposers - Set the amount of proposers you want to start
start(Mode, ConfigAcceptors, ConfigProposers,Sleep, NumAcceptors, NumProposers) ->

  validateData(Sleep, NumProposers),

  {AcceptorNames, AccRegister} = calculateAcceptorsNames(NumAcceptors),
 
  {ProposerNames, PropInfo} = calculateProposerNames(NumProposers), 
  
  register(gui, spawn(node(),fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      case Mode of
        remote ->
          AccRegisterP = lists:map(fun(Elem) -> {Elem,acc_node()} end, AccRegister),
          start_acceptors(remote, ConfigAcceptors, AccIds, AccRegister),
          start_proposers(remote, ConfigProposers,PropIds, PropInfo, AccRegisterP, Sleep);
        local ->
          start_acceptors(local, ConfigAcceptors, AccIds, AccRegister),
          start_proposers(local, ConfigProposers, PropIds, PropInfo, AccRegister, Sleep) 
        end          
  end,
  true.


start_acceptors(Mode, ConfigAcceptors, AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      case Mode of
        remote ->
          register(RegName, acceptor:start(acc_node(), RegName, AccId, ConfigAcceptors));
        local ->
          register(RegName, acceptor:start(RegName, AccId, ConfigAcceptors))
      end,         
      start_acceptors(Mode, ConfigAcceptors, Rest, RegNameRest)
  end.

start_proposers(Mode, ConfigProposers, PropIds, PropInfo, Acceptors, Sleep) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      case Mode of
        remote ->
          proposer:start(node(), RegName, Colour, Acceptors, FirstSleep, PropId,ConfigProposers);
        local ->
          proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, ConfigProposers)
      end,  
      start_proposers(Mode, ConfigProposers, Rest, RestInfo, Acceptors, RestSleep)
  end.

stop() ->
  Processes = registered(),
  Acceptors = lists:filter(fun(Elem) -> string:prefix(atom_to_list(Elem),"Acceptor") /= nomatch end, Processes),
  lists:foreach(fun(Acc) -> stop(Acc) end, Acceptors),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop
  end.


crash(Name, ConfigAcceptors) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      pers:open(Name),
      {_, _, _, Pn} = pers:read(Name),
      Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
      pers:close(Name),
      unregister(Name),
      exit(Pid, "crash"),
      timer:sleep(2000),
      register(Name, acceptor:start(acc_node(), Name, na, ConfigAcceptors))
  end.

crash(Name) ->
    crash(Name, ?DEF_CONFIG_ACC).

% Calculate Acceptor Node name. 
acc_node() ->
  {_, Hostname} = inet:gethostname(),
  AccNode = "paxy-acc@"++Hostname,
  list_to_atom(AccNode).

% Helper Function used on calculateProposerNames  
fromNumberToProposer(NumProp) ->
    ProposerNames = ["kurtz", "kilgore", "willard","kiro","nala"],
    ProposerColours = [?RED,?YELLOW,?BLUE,?GREEN,?PURPLE],
    RName = rand:uniform(5),
    RColour = rand:uniform(5),
    PropName = lists:nth(RName,ProposerNames)++" - "++integer_to_list(NumProp),
    {"Proposer "++PropName, lists:nth(RColour,ProposerColours), list_to_atom(PropName)}.

% Calculate dynamically Proposers Names and Atoms based on the requested number of proposers
calculateProposerNames(NumPro) ->
    PropRegister = lists:seq(1,NumPro),
    ListCalculateProposers = lists:map(fun(Elem) -> fromNumberToProposer(Elem) end,PropRegister),
    ProposerNames = lists:map(fun({PropName, PropCol, _}) -> {PropName,PropCol} end, ListCalculateProposers),
    PropInfo = lists:map(fun({_, PropCol, PropAtom}) -> {PropAtom,PropCol} end, ListCalculateProposers),
    {ProposerNames, PropInfo}.

% Calculate dynamically names and register atoms of Acceptors based on the requested number
calculateAcceptorsNames(NumAcceptors) ->
  AcceptorNames = [ "Acceptor "++integer_to_list(X) || X <- lists:seq(1,NumAcceptors) ],
  AccRegister = [ list_to_atom(X) || X <- AcceptorNames ],
  {AcceptorNames, AccRegister}.

% Validate Length Sleeps against Number of Proposers
validateData(Sleep, NumProposers) ->
  SleepLength = length(Sleep),
  SleepLength == NumProposers orelse throw("Sleep Array should have the same length as number of Proposers. Sleep Array Length["++integer_to_list(SleepLength)++"] - Proposers["++integer_to_list(NumProposers)++"]").

