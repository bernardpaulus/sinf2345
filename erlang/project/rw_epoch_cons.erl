% @doc Read/Write Epoch Consensus Algorithm
% @author bernard paulus
% @author martin trigaux
-module(rw_epoch_cons).
-compile(export_all).

-import(spawn_utils, [spawn_multiple_on_top/3]).

-record(rwe_state, {
        leader = none,
        peers = [],
        beb = beb,
        link = link,
        ets = epoch_ts,
        tmp_val = bottom,
        states = dict:new(),
        accepted = 0,
        val_ts = val_ts,
        val = val,
        my_ups = []
        }).


% start stuff
-define(base_start_delay, 100).
-define(max_start_delay, 1600).

-record(startrw_state, {
        beb = beb,
        link = link,
        ets = ets,
        epoch_state = e_state,
        propose = none,
        leader = none,
        start_delay = ?base_start_delay,
        replies = sets:new(),
        n = -1
        }).

% @spec (Bebs, Links, Epoch_Ts, E_States) -> Rw_epoch_cons :: [pid()]
%   Bebs = [Beb :: pid()]
%   Links = [Link :: pid()]
%   Epoch_Ts = [integer()]
%   E_States = [{Ts :: integer(), Val :: term()}]
%   N = integer()
% @doc spawns a rw_consensus_epoch instance with the given Epoch_Ts.
start(Bebs, Links, Epoch_Ts, E_States, N) when 
        is_pid(hd(Bebs)), is_pid(hd(Links)), 
        length(Bebs) == length(Links), length(Links) == length(E_States) ->
    spawn_multiple_on_top(Bebs, [fun init/6 || _ <- Bebs], 
            [[Link, Epoch_Ts, E_State, N] || {Link, E_State} 
                <- lists:zip(Links, E_States)]);
    
% @spec (Beb, Link, Epoch_Ts, E_State, N) -> pid()
%   Beb = pid()
%   Link = pid()
%   Epoch_Ts = integer()
%   E_State = {Ts :: integer(), Val :: term()}
%   N = integer()
% @doc spawn a read write epoch consensus process, and discovers peers from
% the same read write epoch consensus. Non-leader nodes wait on the leader to
% enter the core algorithm. Leader waits for a {propose, V}, and then start
% broadcast and wait for ack rounds to ensure enough nodes are started and
% participate in the algorithm.
start(Beb, Link, Epoch_Ts, E_State, N) ->
    start(#startrw_state{
        beb = Beb,
        link = Link,
        ets = Epoch_Ts,
        epoch_state = E_State,
        n = N}).

% @doc waits for a {propose} to know whether self is the leader.
start(State) ->
    Self = self(),
    #startrw_state{ets = Ets} = State,
    receive
        {propose, _} = M ->
            #startrw_state{beb = Beb} = State,
            Beb ! {broadcast, Self, {start, Ets}},
            % wait for answers in start_leader()
            erlang:send_after(?base_start_delay, Self, {start_timeout}),
            start_leader(State#startrw_state{
                propose = M,
                leader = Self});

        {deliver, Leader, {start, Ets}} ->
            #startrw_state{link = Link} = State,
            Link ! {send, Self, Leader, {ack_start, Ets}},
            start_normal(State#startrw_state{
                leader = Leader});

        {abort, Pid, Ets} ->
            Pid ! {aborted, State#startrw_state.epoch_state, Ets}

    end.


start_leader(State) ->
    #startrw_state{start_delay = T, n = N} = State,
    % wait and double timer
    receive {start_timeout} -> Timeout = T * 2 end,
    State1 = leader_empty_buffer(State#startrw_state{
                start_delay = Timeout}),
    #startrw_state{replies = Replies} = State1,
    N_Replies = sets:size(Replies),
    if 
        N_Replies > N / 2 ->
            % ok, let's go!
            #startrw_state{beb = Beb, link = Link, ets = Epoch_Ts, 
                epoch_state = E_State, n = N, propose = Propose} = State1,
            % re-send received propose
            self() ! Propose,
            init([], Beb, Link, Epoch_Ts, E_State, N);

        Timeout =< ?max_start_delay ->
            erlang:send_after(Timeout, self(), {start_timeout}),
            start_leader(State1#startrw_state{
                replies = sets:new()});

        true ->
            throw("start_leader: not enough replies in time")
    end.


leader_empty_buffer(State) ->
    #startrw_state{ets = Ets} = State,
    Leader = Self = self(),
    receive
        {propose, _} -> 
            throw("duplicated propose to leader");

        {deliver, Leader, {start, Ets}} ->
            #startrw_state{link = Link} = State,
            Link ! {send, Self, Leader, {ack_start, Ets}},
            leader_empty_buffer(State);
            
        {deliver, _ , {start, Ets}} ->
            throw("two leaders... die");

        {deliver, Pid, Self, {ack_start, Ets}} ->
            leader_empty_buffer(State#startrw_state{
                replies = sets:add_element(Pid, State#startrw_state.replies)});

        {abort, Pid, Ets} ->
            Pid ! {aborted, State#startrw_state.epoch_state, Ets}

    after 0 ->
        State
    end.

start_normal(State) ->
    #startrw_state{ets = Ets, leader = L} = State,
    Self = self(),
    receive
        {propose, _} -> 
            throw("another process is leader");

        {deliver, Leader, {start, Ets}} when L == none ->
            #startrw_state{link = Link} = State,
            Link ! {send, Self, Leader, {ack_start, Ets}},
            start_normal(State#startrw_state{
                    leader = Leader});

        {deliver, L, {start, Ets}} ->
            #startrw_state{link = Link} = State,
            Link ! {send, Self, L, {ack_start, Ets}},
            start_normal(State);

        {deliver, _Other, {start, Ets}} -> % not none, nor the usual leader
            throw("two or more leaders in rw_epoch_cons");

        {deliver, L, {read, Ets}} = M ->
            % start of the protocol
            #startrw_state{beb = Beb, link = Link, ets = Epoch_Ts, 
                epoch_state = E_State, n = N} = State,
            % resend it to the core algo
            Self ! M,
            init([], Beb, Link, Epoch_Ts, E_State, N);
            
        {abort, Pid, Ets} ->
            Pid ! {aborted, State#startrw_state.epoch_state, Ets}
    end.
    


% @spec (_Peers, Beb, Link, Epoch_Ts, E_State) -> void
%   _Peers = [pid()]
%   Beb = pid()
%   Link = pid()
%   Epoch_Ts = integer()
%   Self_Leader = boolean()
%   N = integer()
init(_Peers, Beb, Link, Epoch_Ts, E_State, N) ->
    Link ! {subscribe, self()},
    Beb ! {subscribe, self()},
    % conditions on state (must call check to test & trigger them)
    condition:start(),
    Self = self(),
    condition:upon(
        % #states > N/2
        fun (#rwe_state{states = E_States}) ->
            dict:size(E_States) > N / 2
        end,
        % action
        fun (_) ->
            Self ! {n_states_over_N_div_2}
        end),
    condition:upon(
        % accepted > N/2
        fun(#rwe_state{accepted = Accepted}) ->
            Accepted > N/2
        end,
        fun(_) ->
            Self ! {accepted_over_N_div_2}
        end),
        
    {Val_Ts, Val} = E_State,
    loop(#rwe_state{
        beb = Beb,
        link = Link,
        ets = Epoch_Ts,
        val_ts = Val_Ts,
        val = Val}).

loop(State) ->
    % Epoch Timestamp included in every message so that two groups of
    % read/write epoch consensus processes don't interfere with each other
    #rwe_state{ets = Ets, leader = Leader} = State,
    Self = self(),
    receive
        {propose, V} -> 
            #rwe_state{beb = Beb} = State,
            % Assume I am the leader
            % Leader = self(), fail if leader is not none or self()
            L = check_leader(self(), State),
            Tmp_Val = V,
            % TODO make other broadcasts, waiting for > N/2 acks to give time to
            % others to start
            Beb ! {broadcast, self(), {read, Ets}},
            loop(State#rwe_state{
                    leader = L,
                    tmp_val = Tmp_Val});

        {deliver, L, {read, Ets}} ->
            #rwe_state{link = Link, val_ts = Val_Ts, val = V} =  State,
            check_leader(L, State),
            Link ! {send, self(), L, {state, Val_Ts, V}},
            loop(State#rwe_state{leader = L});

        {deliver, From, Leader, {state, Val_Ts, V}} ->
            State1 = State#rwe_state{
                    states = dict:append(From, {Val_Ts, V},
                            State#rwe_state.states)},
            % update then check
            condition:check(State1),
            loop(State);

        {n_states_over_N_div_2} when Self == Leader -> 
            % triggered by check() when #states > N/2
            #rwe_state{beb = Beb, states = States} = State,
            case highest(States) of
                {_ , bottom} -> Tmp_Val = State#rwe_state.tmp_val;
                {_, Tmp_Val} -> Tmp_Val
            end,
            Beb ! {broadcast, self(), {write, Tmp_Val, Ets}},
            loop(State#rwe_state{
                    states = dict:new(),
                    tmp_val = Tmp_Val});

        {deliver, Leader, {write, V, Ets}} ->
            #rwe_state{link = Link} = State,
            Link ! {send, self(), Leader, accept},
            loop(State#rwe_state{
                    val_ts = Ets,
                    val = V});

        {deliver, _From, Leader, accept} ->
            State1 = State#rwe_state{
                accepted = State#rwe_state.accepted + 1},
            % update then check
            condition:check(State1),
            loop(State1);

        {accepted_over_N_div_2} when Self == Leader ->
            #rwe_state{beb = Beb, tmp_val = Tmp_Val} = State,
            Beb ! {broadcast, Leader, {decided, Tmp_Val, Ets}},
            loop(State#rwe_state{accepted = 0});

        {deliver, Leader, {decided, V, Ets}} ->
            [Up ! {decide, V, Ets} || Up <- State#rwe_state.my_ups],
            loop(State);

        {abort, Pid, Ets} ->
            #rwe_state{val_ts = Val_Ts, val = Val} = State,
            Pid ! {aborted, {Val_Ts, Val}, Ets};

        {subscribe, Pid} ->
            loop(State#rwe_state{
                my_ups = [Pid | State#rwe_state.my_ups]})
    end.



% @spec (Leader :: pid(), rwe_state) -> Leader :: pid()
% @doc return Leader if the leader is Leader or none.
% Otherwise, fail :)
check_leader(Leader, #rwe_state{leader = none}) -> Leader;
check_leader(Leader, #rwe_state{leader = Leader}) -> Leader.

highest(States) ->
    {_Keys, Vals} = lists:unzip(dict:to_list(States)),
    lists:max(lists:flatten(Vals)).
