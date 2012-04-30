% @doc eager reliable broadcast abstraction
% @author bernard paulus
% @author martin trigaux

-module(erb).
-compile(export_all).

% @type beb_state() = #beb_state{documentation = code}
-record(erb_state, {
        my_up = sets:new(),
        delivered = [],
        down = none}).


erb_loop(State) ->
    Self = self(),
    receive
        {subscribe, Pid, _} ->
            #erb_state{my_up = Ups} = State,
            erb_loop(State#erb_state{
                    my_up = sets:add_element(Pid, Ups)});

        {broadcast, _From, _Msg} -> 
            #erb_state{down = D} = State,
            D ! {broadcast, _From, {data, self(), _Msg}},
            erb_loop(State);

        {deliver, _ , Self, {broadcast, From, Msg}} ->
            #erb_state{delivered = Deli, my_up = Ups, down = Down} = State,
            case lists:filter(fun(N) -> Msg == N end, Deli) of
                [] -> 
                    New_State = State#erb_state{delivered = lists:append([Msg], Deli)},
                    [Up ! {deliver, From, Msg} || Up <- sets:to_list(Ups)],
                    Down ! {broadcast, From, {data, self(), Msg}},
                    erb_loop(New_State);
                true -> erb_loop(State)
            end
    end.
