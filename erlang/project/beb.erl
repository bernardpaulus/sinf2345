% @doc best effort broadcast abstraction
% @author bernard paulus

-module(beb).
-import(spawn_utils, [spawn_multiple_on_top/2]).
-compile(export_all).

-record(beb_state, {
        my_up = sets:new(),
        others = [],
        down = none,
        seq = 1}).

start(Downs) when is_pid(hd(Downs)) ->
    spawn_multiple_on_top(Downs, [fun init/2 || 
            _ <- lists:seq(1,length(Downs))]).

init(Others, Down) ->
    Down ! {subscribe, self(), 0},
    beb_loop(#beb_state{others = Others, down = Down}).

beb_loop(State) ->
    Self = self(),
    receive
        {subscribe, Pid, _} ->
            #beb_state{my_up = Ups} = State,
            beb_loop(State#beb_state{
                    my_up = sets:add_element(Pid, Ups)});

        {broadcast, _From, _Msg, _Seq} = M -> 
            #beb_state{down = D, others = Others, seq = Seq_Num} = State,
            [D ! {send, self(), Other, Seq, M} || {Other, Seq} <-
                    lists:zip(Others, lists:seq(Seq_Num, Seq_Num +
                    length(Others) - 1))],
            beb_loop(State#beb_state{
                    seq = Seq_Num + length(Others)});

        {deliver, _ , Self, _, {broadcast, From, Msg, Seq_Msg}} ->
            #beb_state{my_up = Ups} = State,
            [Up ! {deliver, From, Msg, Seq_Msg} || Up <- sets:to_list(Ups)],
            beb_loop(State)
    end.
