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
    meld_loop(#eld_state{peers = Peers, p2p_link = Link}).

meld_loop(State) ->
    Self= self(),
    receive
        % add a process to the eligeable list
        {subscribe, Pid} ->
            #eld_state{my_up = Up} = State,
            % check avoid double subscription
            case sets:is_element(Pid, Up) of
                true ->
                    io:format("The PID ~p has already subscribed to me (~p) ~n", [Pid, Self]),
                    meld_loop(State);
                false ->
                    meld_loop(State#eld_state{my_up = sets:add_element(Pid, Up)})
            end;

        % add a pid and its subscribers to the list of considered dead
        {suspect, _From, Subscribers} ->
            #eld_state{leader = Leader, peers=Peers, suspected=Suspected, my_up = My_Up} = State,
            case sets:is_element(Leader, Subscribers) of
                true ->
                    io:format("The Leader ~p is dead. Long live the Leader !~n", [Leader]),
                    New_Leader = max_rank(Peers, sets:subtract(sets:from_list(Peers), Suspected)),
                    [ Up ! {trust, Self, New_Leader} 
                        || Up <- sets:to_list(My_Up) ],
                    meld_loop(State#eld_state{
                                suspected = sets:union(Subscribers, Suspected),
                                leader = New_Leader});
                false ->
                    % don't care
                    meld_loop(State#eld_state{
                                suspected = sets:union(Subscribers, Suspected)})
            end;

        % oups it seems you are not dead after all, welcome back
        {restore, _From, Subscribers} ->
            #eld_state{suspected = Suspected, peers = Peers, leader = Leader, my_up = My_Up} = State,
            Resurect_Leader = max_rank(Peers, sets:subtract(sets:from_list(Peers), sets:subtract(Suspected, Subscribers))),
            case Leader == Resurect_Leader of
                true ->
                    % the resurection had no effect on the leader election
                    ok;
                false ->
                    % we have a new leader !
                    [ Up ! {leader, Self, Resurect_Leader} 
                        || Up <- sets:to_list(My_Up) ],
                    ok
            end,
            meld_loop(State#eld_state{
                        suspected = sets:subtract(Suspected, Subscribers),
                        leader = Resurect_Leader})

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
