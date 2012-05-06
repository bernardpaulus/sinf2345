% @doc Leader-Driven Consensus
% based on algo 5.7 p 225
% @author bernard paulus
% @author martin trigaux

-module(ld_cons).
-import(spawn_utils, [spawn_multiple_on_top/3]).
-compile(export_all).

-record(ldc_state, {
          val = none,
          proposed = false,
          decided = false,
          ets = 0,
          lead = none,
          newts = 0,
          newl = none,
          epoch_cons = none,
          epoch_chang = none,
          my_ups = [],
          peers = [],
          round = 0
          }).


start(Epoch_Conss, Epoch_Changes) when
      is_pid(hd(Epoch_Conss)),
      is_pid(hd(Epoch_Changes)),
      length(Epoch_Conss) == length(Epoch_Changes) ->
    spawn_multiple_on_top(Epoch_Conss, [fun init/3 || _ <- Epoch_Conss],
                          [[Epoch_Chang] || Epoch_Chang <- Epoch_Changes]).


% @equiv init(Peers, Epoch_Cons, Epoch_Chang, 0)
init(Peers, Epoch_Cons, Epoch_Chang) ->
    init(Peers, Epoch_Cons, Epoch_Chang, 0).

%% Epoch_Chang : a Monarchical Eventual Leader Detector
%% Peers : peers in the monarch_eld
init(Peers, Epoch_Cons, Epoch_Chang, Round) ->
    Self = self(),
    L0 = monarch_eld:max_rank(Peers, sets:from_list(Peers)),
    Epoch_Cons ! {subscribe, self()},
    Epoch_Chang ! {subscribe, self()},

    %% init propose condition
    condition:start(),
    condition:upon(
      % condition
      fun (#ldc_state{lead= L, val = Val, proposed = Prop}) ->
              (L == Self) and %% pid of Self != pid of self() in the current process
              (Val /= none) and
              (Prop == false)
      end,
      % action
      fun (_) ->            
              Self ! {propose_val_condition}
      end),

    %% create initial EpochConsensus
    ldc_loop(#ldc_state{val = none,
                        proposed = false,
                        decided = false,
                        epoch_cons = Epoch_Cons,
                        epoch_chang = Epoch_Chang,
                        ets = 0,
                        lead = L0,
                        newts = 0,
                        newl = none,
                        peers = Peers,
                        round = Round}).


%% TODO add restart/reinit
%%      debug total order broadcast
%%      write module above tob
ldc_loop(State) ->
    Self= self(),
    receive
        %% old simple
        {subscribe, Pid} ->
            link(Pid),
            ldc_loop(State#ldc_state{
                       my_ups = [Pid | State#ldc_state.my_ups]});

        %% with From and ack
        {subscribe, From, Pid} = M ->
            link(Pid),
            From ! {ack, self(), M},
            ldc_loop(State#ldc_state{
                       my_ups = [Pid | State#ldc_state.my_ups]});
        
        %% Propose a new value for the consensus
        {propose, Val} ->
            condition:check(State#ldc_state{val = Val}),
            ldc_loop(State#ldc_state{val = Val});
        
        %% There is a new epoch, abort
        {startepoch, New_TS, New_L} ->
            #ldc_state{epoch_cons = EC, newts = Old_Ts} = State,
            EC ! {abort, Self, Old_Ts},
            ldc_loop(State#ldc_state{newts = New_TS, newl = New_L});
        
        %% The EpochConsensus has been aborted
        {aborted, Abo_State, Abo_TS} when State#ldc_state.ets == Abo_TS ->
            #ldc_state{epoch_cons = EC, newts = New_TS, newl = New_L} = State,
            %% initialize a new instance of epoch consensus with timestamp ets
            %% Epoch_Cons = rw_epoch_cons:start(Beb, Link, New_TS, Abo_State),
            Epoch_Cons = rw_epoch_cons:reinit(EC, New_TS, Abo_State),
            New_State  = State#ldc_state{proposed = false, ets = New_TS, lead = New_L, epoch_cons = Epoch_Cons},
            condition:check(New_State),
            ldc_loop(New_State);

        %% Decide on a value
        {decide, Val, Ets} when State#ldc_state.ets == Ets ->
            #ldc_state{decided = D, my_ups = My_Ups, round = Round} = State,
            case D of
                false ->
                    io:format("~p decided ~p ~n", [self(), Val]),
                    [Up ! {decide, Val, Round} || Up <- My_Ups],
                    ldc_loop(State#ldc_state{decided = true});
                true -> % should never be true
                    ldc_loop(State)
            end;

        %% condition validated
        %% l = self ^ val != none ^ proposed = false
        {propose_val_condition} ->
            #ldc_state{epoch_cons = EC, val = Val} = State,
            EC ! {propose, Val},
            ldc_loop(State#ldc_state{proposed = true});
        

        %% reinitalize the LDC
        {reinit, Pid, New_Round} = M ->
            #ldc_state{peers = Peers, epoch_cons = Epoch_Cons, 
                epoch_chang = Epoch_Chang, my_ups = My_Ups} = State,
            Target = self(),
            spawn(fun() -> % re-subscribe my ups
                          [utils:subscribe(Up, Target) || Up <- My_Ups],
                          Pid ! {ack, Target, M} % only ack after re-subscription
                  end),
            init(Peers, Epoch_Cons, Epoch_Chang, New_Round)
    end.



reinit(LDC, Round) ->
    M = LDC ! {reinit, self(), Round},
    receive {ack, LDC, M} -> LDC end.
