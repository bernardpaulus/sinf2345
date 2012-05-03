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
          bebs = [],
          links = []
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
        %% %% add a Leader driven consensus to the list of ups
        %% {subscribe, Pid} ->
        %%     #epoch_state{my_up = Up} = State,
        %%     %% check avoid double subscription
        %%     case sets:is_element(Pid, Up) of
        %%         true ->
        %%             io:format("The PID ~p has already subscribed to me (~p) ~n", [Pid, Self]),
        %%             epoch_loop(State);
        %%         false ->
        %%             epoch_loop(State#epoch_state{my_up = sets:add_element(Pid, Up)})
        %%     end;

        {propose, Val} ->
            ldc_loop(State#ldc_state(val = Val));
        
        {startepoch, New_TS, New_L} ->
            #ldc_state(epoch_cons = EC) = State,
            EC ! {abort, Self, New_TS},
            ldc_loop(State#ldc_state(newts = New_TS, newl = New_L));
        
        {aborted, Ao_State, Ao_TS} ->
            #ldc_state(epoch_cons = EC, newts = New_TS, newl = New_L) = State,
            rw_epoch_cons:start(Bebs, Links, New_TS, New_L),
            ldc_loop(#ldc_state{proposed = false, ets = New_TS, newl = New_L});

    end.
