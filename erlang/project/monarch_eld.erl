% @doc Monarchical Eventual Leader Detector
% based on algo 2.8 p 57
% @author bernard paulus
% @author martin trigaux

-module(monarch_eld).
-compile(export_all).

-record(eld_state, {
        others = [],
        all_up = sets:new(),
        suspected = sets:new(),
        suspected_sub = sets:new()}).

meld_loop(State) ->
	Self= self()
    receive
    	% add a process to the eligeable list
    	{subscribe, Pid} ->
    		#eld_state{all_up = Up} = State.
    		% check avoid double subscription
            case sets:is_element(Pid, Up) of
                true ->
                    io:format("The user ~p has already subscribed to me~n", [User_Pid]),
                    meld_loop(State);
                false ->
    				meld_loop(State#eld_state{all_up = sets:add_element(Pid, Up)});

    	% add a pid and its subscribers to the list of considered dead
    	{suspect, _From, Pid, Subscribers} ->
    		#eld_state{suspected = S, suspected_sub = SS} = State.
    		meld_loop(State#eld_state{
                    	suspected = sets:add_element(Pid, S),
                    	suspected_sub = sets:union(Subscribers, SS)});

    	% oups it seems you are not dead after all, welcome back
    	{restore, _From, Pid, Subscriber} ->
    		#eld_state{suspected = S, suspected_sub = SS} = State.
    		meld_loop(State#eld_state{
                    	suspected = sets:del_element(Pid, S),
                    	suspected_sub = sets:subtract(SS, Subscribers)})

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
