% @doc Read/Write Epoch Consensus Algorithm
% @author bernard paulus
% @author martin trigaux
-module(rw_epoch_cons).
-compile(export_all).

-import(spawn_utils, [spawn_multiple_on_top/2]).

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

% spawn a single process TODO spec: cf start() below
%start(Beb, Link, Epoch_Ts, State) ->

% @spec (Bebs, Links, Epoch_Ts, States) -> Rw_epoch_cons :: [pid()]
%   Bebs = [Beb :: pid()]
%   Links = [Link :: pid()]
%   Epoch_Ts = [integer()]
%   States = [{Ts :: integer(), Val :: term()}]
% @doc spawns a rw_consensus_epoch instance with the given Epoch_Ts.
start(Bebs, Links, Epoch_Ts, States) when 
        is_pid(hd(Bebs)), is_pid(hd(Links)), 
        length(Bebs) == length(Links), length(Links) == length(States) ->
    spawn_multiple_on_top(Bebs, Epoch_Ts).
    

% @spec (Peers, Beb, Link, Epoch_Ts, State) -> void
%   Peers = [pid()]
%   Beb = pid()
%   Link = pid()
%   Epoch_Ts = integer()
%   Self_Leader = boolean()
init(Peers, Beb, Link, Epoch_Ts, State) ->
    Link ! {subscribe, self()},
    Beb ! {subscribe, self()},
    % conditions on state (must call check to test & trigger them)
    condition:start(),
    N = length(Peers),
    Self = self(),
    condition:upon(
        % #states > N/2
        fun (#rwe_state{states = States}) ->
            dict:size(States) > N / 2
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
        
    {Val_Ts, Val} = State,
    loop(#rwe_state{
        peers = Peers,
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
            Pid ! {aborted, {Val_Ts, Val, Ets}};

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
