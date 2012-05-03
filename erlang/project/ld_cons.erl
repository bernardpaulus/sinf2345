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
          epoch_chang = none,
          beb = none,
          link = none
          }).

%% Epoch_Chang : a Monarchical Eventual Leader Detector
%% Peers : peers in the monarch_eld
%% Link : a p2p link
%% Be : a Best effort broadcast
init(Epoch_Chang, Peers, Link, Beb) ->
    %% Bebs = beb:start([Node()]),
    %% Links = link:damn_simple_link([Node()]),
    L0 = monarch_eld:max_rank(Peers, Peers),
    %% create initial EpochConsensus
    Epoch_Cons = rw_epoch_cons:start(Beb, Link, 0, L0),
    ldc_loop(#ldc_state{val = none, proposed = false,
                        decided = false, epoch_cons = Epoch_Cons,
                        epoch_chang = Epoch_Chang, beb = Beb}).

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
            #ldc_state{newts = New_TS, newl = New_L} = State,
            %% initialize a new instance of epoch consensus with timestamp ets
            %% Epoch_Cons = rw_epoch_cons:start(Beb, Link, New_TS, Abo_State),
            Epoch_Cons = super:reinit(New_TS, Abo_State),
            ldc_loop(State#ldc_state{proposed = false, ets = New_TS, newl = New_L, epoch_cons = Epoch_Cons});

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
