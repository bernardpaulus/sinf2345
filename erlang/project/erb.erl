% @doc eager reliable broadcast abstraction
% based on algo 3.3 p80
% @author bernard paulus
% @author martin trigaux

-module(erb).
-import(spawn_utils, [spawn_multiple_on_top/2]).
-compile(export_all).

% @type erb_state() = #erb_state{documentation = code}
-record(erb_state, {
        my_up = sets:new(),
        delivered = [],
        down = none}).

start(Downs) when is_pid(hd(Downs)) ->
    spawn_multiple_on_top(Downs, [fun init/2 || 
            _ <- lists:seq(1,length(Downs))]);

start(Nodes) when is_atom(hd(Nodes)) ->
    start(erb:start(Nodes));

start([]) -> [].

% @spec (Down :: pid()) -> void
% @doc initializes the erb process
init(_Others, Down) ->
    Down ! {subscribe, self()},
    erb_loop(#erb_state{down = Down}).


erb_loop(State) ->
    Self = self(),
    receive
        {subscribe, Pid} ->
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
