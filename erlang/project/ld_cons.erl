% @doc Leader-Driven Consensus
% based on algo 5.7 p 225
% @author bernard paulus
% @author martin trigaux

-module(ld_cons).
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
          epoch_chang = none
          }).


start(Epoch_Conss, Epoch_Changes) when
      is_pid(hd(Epoch_Conss)),
      is_pid(hd(Epoch_Chang)),
      length(Epoch_Conss) == length(Epoch_Changes) ->
    spawn_multiple_on_top(Epoch_Conss, [fun init/3 || _ <- Epoch_Conss],
                          [[Epoch_Chang] || Epoch_Chang <- Epoch_Changes]).


%% Epoch_Chang : a Monarchical Eventual Leader Detector
%% Peers : peers in the monarch_eld
init(Peers, Epoch_Cons, Epoch_Chang) ->
    Self = self(),
    L0 = monarch_eld:max_rank(Peers, sets:from_list(Peers)),
    Epoch_Cons ! {suscribe, self()},
    Epoch_Chang ! {suscribe, self()},

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
                        newl = none}).


ldc_loop(State) ->
    Self= self(),
    receive
        %% Propose a new value for the consensus
        {propose, Val} ->
            condition:checl(State#ldc_state{val = Val}),
            ldc_loop(State#ldc_state{val = Val});
        
        %% There is a new epoch, abort
        {startepoch, New_TS, New_L} ->
            #ldc_state{epoch_cons = EC} = State,
            EC ! {abort, Self, New_TS},
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
            #ldc_state{decided = D} = State,
            case D of
                false ->
                    %% TOFIX what shoud I do with that value ?
                    io:format("We have decided on value ~p ~n", [Val]),
                    ldc_loop(State#ldc_state{decided = true});
                true ->
                    ldc_loop(State)
            end;

        %% condition validated
        %% l = self ^ val != none ^ proposed = false
        {propose_val_condition} ->
            #ldc_state{epoch_cons = EC, val = Val} = State,
            EC ! {propose, Val},
            ldc_loop(State#ldc_state{proposed = true})            
    end.
