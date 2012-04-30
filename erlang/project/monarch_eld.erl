% @doc Monarchical Eventual Leader Detector
% @author bernard paulus
% @author martin trigaux

-module(monarch_eld).
-compile(export_all).

-record(eld_state, {
        others = [],
        all_up = dict:new(),
        my_up = 

loop(State) ->
    receive
        
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
