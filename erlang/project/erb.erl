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
    %io:format("Je suis erb ~p~n", [self()]),
    Down ! {subscribe, self()},
    erb_loop(#erb_state{down = Down}).


erb_loop(State) ->
    receive
        %% old simple
        {subscribe, Pid} ->
            #erb_state{my_up = Ups} = State,
            link(Pid),
            erb_loop(State#erb_state{
                    my_up = sets:add_element(Pid, Ups)});

        %% with From and ack
        {subscribe, From, Pid} = M ->
            link(Pid),
            From ! {ack, self(), M},
            #erb_state{my_up = Ups} = State,
            erb_loop(State#erb_state{
                    my_up = sets:add_element(Pid, Ups)});

        {broadcast, From, Msg} -> 
            #erb_state{down = D} = State,
            D ! {broadcast, From, {data, self(), Msg}},
            erb_loop(State);

        %% {deliver, _From_Beb, {data, Self, {{broadcast, From_Up, Msg}, _Seq}}} ->
        {deliver, _From_Beb, {data, From_Up, Msg}} ->
            #erb_state{delivered = Deli, my_up = Ups, down = Down} = State,
            %% Does Msg belongs to delivered
            case lists:filter(fun(N) -> Msg == N end, Deli) of
                [] -> 
                    New_State = State#erb_state{delivered = lists:append([Msg], Deli)},
                    [Up ! {deliver, From_Up, Msg} || Up <- sets:to_list(Ups)],
                    Down ! {broadcast, self(), {data, From_Up, Msg}},
                    erb_loop(New_State);
                _ -> erb_loop(State)
            end
    end.
