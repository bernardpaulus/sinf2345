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
    L0 = monarch_eld:max_rank(Peers, sets:from_list(Peers)),
    Epoch_Cons ! {subscribe, self()},
    Epoch_Chang ! {subscribe, self()},

    %% init propose condition
    condition:start(),
    insert_condition(),

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

insert_condition() ->
    Self = self(),
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
      end).

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
            condition:clear(),
            insert_condition(),
            condition:check(State#ldc_state{val = Val}),
            ldc_loop(State#ldc_state{val = Val});
        
        %% There is a new epoch, abort
        {startepoch, New_TS, Epoch_Chang_L} ->
            io:format("~p start epoch ~p ~p~n", [Self, New_TS, Epoch_Chang_L]),
            #ldc_state{epoch_cons = EC, newts = Old_Ts, 
                epoch_chang = Epoch_Chang} = State,
            EC ! {abort, Self, Old_Ts},
            New_L = leader(Epoch_Chang_L, Epoch_Chang),
            ldc_loop(State#ldc_state{newts = New_TS, newl = New_L});
        
        %% The EpochConsensus has been aborted
        {aborted, Abo_State, Abo_TS} when State#ldc_state.ets == Abo_TS ->
            #ldc_state{epoch_cons = EC, newts = New_TS, newl = New_L} = State,
            %% initialize a new instance of epoch consensus with timestamp ets
            %% Epoch_Cons = rw_epoch_cons:start(Beb, Link, New_TS, Abo_State),
            %empty_msg_queue(),
            Epoch_Cons = rw_epoch_cons:reinit(EC, New_TS, Abo_State),
            New_State  = State#ldc_state{proposed = false, ets = New_TS, 
                            lead = New_L, epoch_cons = Epoch_Cons, val = none, 
                            decided = false},
            condition:clear(),
            insert_condition(),
            condition:check(New_State),
            ldc_loop(New_State);

        %% Decide on a value
        {decide, Val, Ets} when State#ldc_state.ets == Ets ->
            io:format("~p receive decide ~p ~n", [self(), Val]),
            self() ! {test},
            #ldc_state{decided = D, my_ups = My_Ups, round = Round} = State,
            case D of
                false ->
                    io:format("~p decided ~p ~n", [self(), Val]),
                    [Up ! {decide, Val, Round} || Up <- My_Ups],
                    ldc_loop(State#ldc_state{decided = true, val = none})
            end;

        %% condition validated
        %% l = self ^ val != none ^ proposed = false
        {propose_val_condition} ->
            #ldc_state{epoch_cons = EC, val = Val} = State,
            EC ! {propose, Val},
            ldc_loop(State#ldc_state{proposed = true});
        

        %% reinitalize the LDC
        {reinit, Pid, New_Round} = M ->
            #ldc_state{epoch_cons = Epoch_Cons, epoch_chang = Epoch_Chang, my_ups = My_Ups, newts = Old_Ts} = State,
            % empty msg queue
            empty_msg_queue(),
            % forcibly start a new epoch
            {New_TS, Epoch_Chang_L} = epoch_change:reinit(Epoch_Chang),
            New_L = leader(Epoch_Chang_L, Epoch_Chang),
            % reinit the rw epoch consensus
            Epoch_Cons ! {abort, Self, Old_Ts},
            receive {aborted, Abo_State, Abo_TS} when Abo_TS == Old_Ts -> ok end,
            EC = rw_epoch_cons:reinit(Epoch_Cons, New_TS, Abo_State),
            Target = self(),
            spawn(fun() -> % re-subscribe my ups
                          [utils:subscribe(Up, Target) || Up <- My_Ups],
                          Pid ! {ack, Target, M} % only ack after re-subscription
                  end),
            % reset the conditions
            condition:clear(),
            insert_condition(),
            ldc_loop(State#ldc_state{proposed = false, decided = false, ets = New_TS, lead = New_L,
                    newts = New_TS, newl = New_L, epoch_cons = EC, val = none,
                    round = New_Round})
    end.

empty_msg_queue() ->
    receive _ -> empty_msg_queue() after 0 -> ok end.

reinit(LDC, Round) ->
    M = LDC ! {reinit, self(), Round},
    receive {ack, LDC, M} -> LDC end.

% @spec (Leader_Below, Epoch_Chang :: pid()) -> Self :: pid() | Leader_Below
%   Leader_Below = pid()
% @doc returns self() if Leader_Below is our Epoch_Chang
leader(Leader_Below, Epoch_Chang) ->
    if 
        Leader_Below == Epoch_Chang -> 
            % our Epoch change is the leader, thus we are
            self();
        true ->
            Leader_Below % not pretty, but /= self()
    end.
