% @doc Consensus based total order broadcast
% @author bernard paulus
% @author martin trigaux

-module(tob).
-import(spawn_utils, [spawn_multiple_on_top/3]).
-compile(export_all).

% messages need to be unique here: decision of the ones to deliver (or not) is
% based on intersections of sets. 
% {Peer, Seq_of_peer} is unique if Peer increments it's Seq

-record(tob_state, {
        unordered = sets:new(),
        delivered = sets:new(),
        round = 1,
        wait = false,
        rb = rb,
        consensus = cons,
        seq = 0,
        my_ups = []}).

start(RBs, Consensuss) 
        when is_pid(hd(RBs)), is_pid(hd(Consensuss)),
        length(RBs) == length(Consensuss) ->
    spawn_multiple_on_top(RBs, [fun init/3 || _ <- RBs], Consensuss).

start(Nodes) ->
    Links = link:perfect_link(Nodes),
    Bebs = beb:start(Links),
    RBs = erb:start(Bebs),
    FDs = inc_timeout_fd:start(Links),
    LDs = monarch_eld:start(FDs, Links),
    RW_Epochs_Cons = rw_epoch_cons:start(Bebs, Links, 1, [{1, bottom} || _ <- Bebs]),
    Epoch_Changes = epoch_change:start(LDs, Bebs, Links),
    Consensuss = ld_cons:start(RW_Epochs_Cons, Epoch_Changes),
    start(RBs, Consensuss).

init(_Peers, RB, Consensus) when is_pid(RB) ->
    init(#tob_state{rb = RB, consensus = Consensus}).

init(State) ->
    Self = self(),
    % TODO subscribe
    condition:start(),
    condition:upon(
        % Unordered not empty and wait is false
        fun(#tob_state{unordered = Unordered, wait = Wait}) ->
            (Unordered /= []) and (Wait == false)
        end,
        % send message
        fun(_State) ->
            Self ! {unordered_not_empty_and_wait_false}
        end),
    loop(State).
    
loop(State) ->
    receive 
        {broadcast, _From, _Msg} = M ->
            #tob_state{seq = Seq, rb = RB} = State,
            RB ! {broadcast, self(), {M, Seq}},
            loop(State#tob_state{
                    seq = Seq + 1});

        {deliver, Peer, {{broadcast, From, Msg}, Seq}} ->
            #tob_state{unordered = Unordered, delivered = Delivered} = State,
            M = {Peer, Seq, From, Msg},
            case sets:is_element(M, Delivered) of
                false -> 
                    State1 = State#tob_state{
                        unordered = sets:add_element(M, Unordered)},
                    condition:check(State1),
                    loop(State1);
                true -> 
                    loop(State)
            end;

        {unordered_not_empty_and_wait_false} ->
            #tob_state{consensus = Consensus, unordered = Unordered} = State,
            % initialize a new instance c.round of consensus
            % ld_cons:reinit(Round), % TODO 
            Consensus ! {propose, Unordered},
            loop(State#tob_state{wait = true});

        {decide, Decided, Round} -> % {decide, V, Round}
            #tob_state{delivered = Delivered, unordered = Unordered, 
                my_ups = My_Ups, round = Round, wait = true} = State,
            % forall in sort(decided)
            [
                % trigger deliver to all subscribers
                [ Up ! {deliver, From, Msg} || Up <- My_Ups]
            || {_Peer, _Seq, From, Msg} <- lists:sort(sets:to_list(Decided))],
            % TODO reinit consensus
            loop(State#tob_state{
                delivered = sets:union(Delivered, Decided),
                unordered = sets:subtract(Unordered, Decided),
                round = Round + 1,
                wait = false});

        % old subscribe
        {subscribe, Pid} ->
            loop(State#tob_state{
                my_ups = [Pid | State#tob_state.my_ups]});

        % added ack to subscriptions
        {subscribe, From, Pid} = M ->
            From ! {ack, self(), M},
            loop(State#tob_state{
                my_ups = [Pid | State#tob_state.my_ups]})
    end.
