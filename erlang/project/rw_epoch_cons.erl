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
                <- lists:zip(Links, E_States)]).
    

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
    Self = self(),
    % conditions on state (must call check to test & trigger them)
    condition:start(),
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

reinit(Pid, Ets, E_State) ->
    % TODO
    {Pid, Ets, E_State}.

loop(State) ->
    % Epoch Timestamp included in every message so that two groups of
    % read/write epoch consensus processes don't interfere with each other
    #rwe_state{ets = Ets, leader = Leader} = State,
    Self = self(),
    receive
        % request from upper layer done only to the leader
        {propose, V} -> 
            #rwe_state{beb = Beb} = State,
            % Leader = self(), fail if leader is not none or self()
            L = check_leader(self(), State),
            Tmp_Val = V,
            Beb ! {broadcast, self(), {read, Ets}},
            loop(State#rwe_state{
                    leader = L,
                    tmp_val = Tmp_Val});

        % respond with our state to the {read, } broadcasted by leader
        {deliver, L, {read, Ets}} ->
            #rwe_state{link = Link, val_ts = Val_Ts, val = V} =  State,
            check_leader(L, State),
            Link ! {send, self(), L, {state, Val_Ts, V}},
            loop(State#rwe_state{leader = L});

        % leader only: receive the different states
        {deliver, From, Leader, {state, Val_Ts, V}} ->
            State1 = State#rwe_state{
                    states = dict:append(From, {Val_Ts, V},
                            State#rwe_state.states)},
            % update then check
            condition:check(State1),
            loop(State);

        % leader: when received enough states, write the highest to all nodes
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

        % all nodes update their states
        {deliver, Leader, {write, V, Ets}} ->
            #rwe_state{link = Link} = State,
            Link ! {send, self(), Leader, accept},
            loop(State#rwe_state{
                    val_ts = Ets,
                    val = V});

        % leader: counts the number of nodes with the new state
        {deliver, _From, Leader, accept} ->
            State1 = State#rwe_state{
                accepted = State#rwe_state.accepted + 1},
            % update then check
            condition:check(State1),
            loop(State1);

        % leader: when majority of nodes got the new state
        {accepted_over_N_div_2} when Self == Leader ->
            #rwe_state{beb = Beb, tmp_val = Tmp_Val} = State,
            Beb ! {broadcast, Leader, {decided, Tmp_Val, Ets}},
            loop(State#rwe_state{accepted = 0});

        % each node indicate their ups the leader decided upon a value
        {deliver, Leader, {decided, V, Ets}} ->
            [Up ! {decide, V, Ets} || Up <- State#rwe_state.my_ups],
            loop(State);

        % aborted on new epoch. Can be at any step of the algo
        {abort, Pid, Ets} ->
            #rwe_state{val_ts = Val_Ts, val = Val} = State,
            Pid ! {aborted, {Val_Ts, Val}, Ets};


        % ho no, an undocumented message!
        {subscribe, Pid} ->
            loop(State#rwe_state{
                my_ups = [Pid | State#rwe_state.my_ups]})
    end.



% @spec (Leader :: pid(), rwe_state) -> Leader :: pid()
% @doc return Leader if the leader is Leader or none.
% Otherwise, fail :)
check_leader(Leader, #rwe_state{leader = none}) -> Leader;
check_leader(Leader, #rwe_state{leader = Leader}) -> Leader.

% @spec (States ) -> Max_Val :: term()
%   States = dict()
% @doc returns the maximum value of the values of States.
%
% not exactly the same as highest() of Cachin/Guerraoui: theirs assumed the
% values where {Ts, Val}, and compared only on Ts, assuming two vals with the
% same Ts were equal. Here, lexicographic order is used.
highest(States) ->
    {_Keys, Vals} = lists:unzip(dict:to_list(States)),
    lists:max(lists:flatten(Vals)).
