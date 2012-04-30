% @doc Eventual Perfect Failure Detector by Increasing Timout
% @author paulus bernard
% @author martin trigaux

-module(inc_timeout_fd).
-import(spawn_utils, [spawn_multiple_on_top/2]).
-compile(export_all).

start(Downs) when is_pid(hd(Downs)) ->
    spawn_multiple_on_top(Downs, [fun init/2 || 
            _ <- lists:seq(1,length(Downs))]);

start(Nodes) when is_atom(hd(Nodes)) ->
    start(link:perfect_link(Nodes));

start([]) -> [].

-define(delta, 100).

-record(it_state, {
    down = none,
    others = [],
    alive = sets:new(),
    suspected = sets:new(),
    delay = ?delta,
    my_up = sets:new(),
    all_up = dict:new()
    }).

init(Others, Down) ->
    Down ! {susbcribe, self()},
    erlang:send_after(?delta, self(), {timeout}),
    inc_timeout_loop(#it_state{
        down = Down,
        alive = sets:from_list(Others)}).


inc_timeout_loop(State) ->
    Self = self(),
    receive
        {subscribe, Up} = M ->
            #it_state{down = Down, others=Others, my_up = My_Up, 
                    all_up=All_Up} = State,
            % transmit the request to all others
            [Down ! {send, self(), Other, M} || 
                    Other <- Others, Other /= self()],
            inc_timeout_loop(State#it_state{
                    my_up = sets:add_element(Up, My_Up),
                    all_up = dict:append(Up, self(), All_Up)}); 


        {deliver, Other, Self, {subscribe, Up}} -> 
            All_Up = State#it_state.all_up,
            inc_timeout_loop(State#it_state{
                    all_up = dict:append(Up, Other, All_Up)});


        {timeout} ->
            #it_state{alive = Alive, suspected = Suspected, delay = Delay,
                    others = Others} = State,
            case sets:size(sets:intesection(Alive, Suspected)) of
                X when X > 0 ->
                    State1 = State#it_state{delay = Delay + ?delta};
                0 -> 
                    State1 = State
            end,
            State2 = lists:foldl(fun (P, State_Acc) ->
                    #it_state{suspected = Suspect, down = Down,
                        my_up = My_Ups, all_up = All_Up} = State_Acc,
                    Is_Alive = sets:is_element(P, Alive),
                    Is_Suspect = sets:is_element(P, Suspect),
                    if 
                        (not Is_Alive) and Is_Suspect ->
                            NewS = sets:add_element(P, Suspect),
                            Suscribers = sets:from_list(dict:fetch(P, All_Up)),
                            [Up ! {suspect, P, Suscribers} || Up <- My_Ups];
                        Is_Alive and Is_Suspect ->
                            NewS = sets:del_element(P, Suspect),
                            Suscribers = sets:from_list(dict:fetch(P, All_Up)),
                            [Up ! {restore, P, Suscribers} || Up <- My_Ups];
                        true ->
                            NewS = Suspect
                    end,
                    Down ! {send, Self, P, heartbeat_request},
                    State_Acc#it_state{suspected = NewS}
                end,
                State1, Others),
            State3 = State2#it_state{alive = sets:new()}, % empty alive
            erlang:send_after(State3#it_state.delay, Self, {timeout}),
            inc_timeout_loop(State3);


        {deliver, From, Self, heartbeat_request} ->
            #it_state{down = Down} = State,
            Down ! {send, Self, From, heartbeat_reply},
            inc_timeout_loop(State);


        {deliver, From, Self, heartbeat_reply} ->
            #it_state{alive = Alive} = State,
            inc_timeout_loop(State#it_state{
                alive = sets:add_element(From, Alive)})
    end.
