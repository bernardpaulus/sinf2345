% @doc Monarchical Eventual Leader Detector
% based on algo 2.8 p 57
% @author bernard paulus
% @author martin trigaux

-module(monarch_eld).
-compile(export_all).

-record(eld_state, {
        same_level = [],
        my_up = sets:new(),
        suspected = sets:new()}).

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
