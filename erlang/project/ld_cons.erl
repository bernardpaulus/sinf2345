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

%% Epoch_Chang : a Monarchical Eventual Leader Detector
%% Peers : peers in the monarch_eld
init(Epoch_Cons, Epoch_Chang, Peers) ->
    L0 = monarch_eld:max_rank(Peers, sets:from_list(Peers)),
    Epoch_Cons ! {suscribe, self()},
    Epoch_Chang ! {suscribe, self()},
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


%% TODO upon l = self ^ val...
ldc_loop(State) ->
    Self= self(),
    receive
        %% Propose a new value for the consensus
        {propose, Val} ->
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
            ldc_loop(State#ldc_state{proposed = false, ets = New_TS, lead = New_L, epoch_cons = Epoch_Cons});

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
            end
    end.
