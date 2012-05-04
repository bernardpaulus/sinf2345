% @doc Monarchical Eventual Leader Detector
% based on algo 2.8 p 57
% @author bernard paulus
% @author martin trigaux

-module(monarch_eld).
-compile(export_all).
-import(spawn_utils, [spawn_multiple_on_top/3]).

-record(eld_state, {
          peers = [],
          my_up = sets:new(),
          suspected = sets:new(),
          leader = none,
          p2p_link = none}). % p2p links to up nodes

start(Fail_Dets, Perfect_Links) when 
      is_pid(hd(Fail_Dets)), is_pid(hd(Perfect_Links)),
      length(Fail_Dets) == length(Perfect_Links) ->
                    spawn_multiple_on_top(Fail_Dets, 
                                          [fun init/3 || _ <- lists:seq(1,length(Fail_Dets))],
                                          [[Link] || Link <- Perfect_Links] ).

init(Peers, FD, Link) ->
    Link ! {subscribe, self()},
    FD ! {subscribe, self()},
    % condition
    Self = self(),
    Peers_set = sets:from_list(Peers),
    condition:start(),
    condition:upon(
        fun (#eld_state{leader = Leader, suspected = Suspected}) ->
            Trusted = sets:substract(Peers_set, Suspected),
            case max_rank(Peers, Trusted) of
                Leader -> ok;
                New_Leader -> Self ! {leader_not_trusted, New_Leader}
            end,
            false % trick the condition module into believing condition is
                  % never satisfied
        end,
        fun (_) -> dummy_action end),
    meld_loop(#eld_state{peers = Peers, p2p_link = Link}).

meld_loop(State) ->
    Self= self(),
    receive
        % add a process to the eligeable list
        {subscribe, Pid} ->
            #eld_state{my_up = Up} = State,
            % check avoid double subscription -> no! effect is the same
            meld_loop(State#eld_state{my_up = sets:add_element(Pid, Up)});

        % add a pid and its subscribers to the list of considered dead
        {suspect, _From, Subscribers} ->
            % case sets:is_element(Leader, Subscribers) of % Leader == bottom !
            State1 = State#eld_state{
                        suspected = sets:union(Subscribers, 
                             State#eld_state.suspected)},
            condition:check(State1),
            meld_loop(State1);

        % oups it seems you are not dead after all, welcome back
        {restore, _From, Subscribers} ->
            #eld_state{suspected = Suspected} = State,
            State1 = State#eld_state{
                        suspected = sets:subtract(Suspected, Subscribers)},
            condition:check(State1),
            meld_loop(State1);

        % leader /= max_rank(Peers \ suspected)
        {leader_not_trusted, New_Leader} -> 
            #eld_state{my_up = My_Up} = State,
            [ Up ! {trust, Self, New_Leader} 
                || Up <- sets:to_list(My_Up) ],
            State1 = State#eld_state{
                        leader = New_Leader},
            condition:check(State1),
            meld_loop(State1)
            
    end.

% @spec ([pid()], Trusted :: sets:set()) -> 
%   Leader :: pid() | {error, no_process_trusted}
% @doc returns the first trusted process
max_rank([], _Trusted) -> {error, no_process_trusted};
max_rank([H | T], Trusted) ->
    case sets:is_element(H, Trusted) of
        true -> H;
        false -> max_rank(T, Trusted)
    end.
