% @doc Monarchical Eventual Leader Detector
% based on algo 2.8 p 57
% @author bernard paulus
% @author martin trigaux

-module(monarch_eld).
-compile(export_all).

-record(eld_state, {
        same_level = [],
        my_up = sets:new(),
        suspected = sets:new(),
        leader = nil}).


start(Fail_Dets, Perfect_Links) when 
        is_pid(hd(Fail_Dets)), is_pid(hd(Perfect_Links)),
        length(Fail_Dets) == length(Perfect_Links) ->
    spawn_multiple_on_top(Fail_Dets, 
            [fun init/3 || _ <- lists:seq(1,length(Downs))],
            [[Link] || Link <- Perfect_Links ).

init(Peers, FD, Link) ->
    Link ! {subscribe, self()},
    FD ! {subscribe, self()},
    % TODO

meld_loop(State) ->
    Self= self()
    receive
        % add a process to the eligeable list
        {subscribe, Pid} ->
            #eld_state{my_up = Up} = State.
            % check avoid double subscription
            case sets:is_element(Pid, Up) of
                true ->
                    io:format("The PID ~p has already subscribed to me (~p) ~n", [Pid, Self]),
                    meld_loop(State);
                false ->
                meld_loop(State#eld_state{my_up = sets:add_element(Pid, Up)});

        % add a pid and its subscribers to the list of considered dead
        {suspect, _From, Subscribers} ->
            #eld_state{suspected = S} = State.
            meld_loop(State#eld_state{
                        suspected = sets:union(Subscribers, S)});

        % oups it seems you are not dead after all, welcome back
        {restore, _From, Subscriber} ->
            #eld_state{suspected = S} = State.
            meld_loop(State#eld_state{
                        suspected = sets:subtract(S, Subscribers)})

    end.

% @spec ([pid()], Trusted :: sets:set()) -> 
%   Leader :: pid() | {error, no_process_trusted}
% @doc returns the first trusted process
max_rank([], Trusted) -> {error, no_process_trusted};
max_rank([H | T], Trusted) ->
    case sets:is_element(H, Trusted) of
        true -> H;
        false -> max_rank(H, Trusted)
    end.
