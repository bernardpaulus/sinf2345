% @doc Leader-based Epoch Change
% based on algo 5.5 p 219
% @author bernard paulus
% @author martin trigaux

-module(epoch).
-compile(export_all).
-import(spawn_utils, [spawn_multiple_on_top/3]).

-record(epoch_state, {
          down = none,
          beb = none,
          trusted = none,
          p2p_link = none,
          my_up = sets:new()}). % p2p links to up nodes

%% start(Fail_Dets, Perfect_Links) when 
%%       is_pid(hd(Fail_Dets)), is_pid(hd(Perfect_Links)),
%%       length(Fail_Dets) == length(Perfect_Links) ->
%%                     spawn_multiple_on_top(Fail_Dets, 
%%                                           [fun init/3 || _ <- lists:seq(1,length(Fail_Dets))],
%%                                           [[Link] || Link <- Perfect_Links] ).

init(Down, Beb, Link) ->
    Link ! {subscribe, self()},
    Beb ! {subscribe, self()},
    Down ! {subscribe, self()},
    epoch_loop(#epoch_state{down = Down, p2p_link = Link, beb = Beb}).

epoch_loop(State) ->
    Self= self(),
    receive
        %% add a Leader driven consensus to the list of ups
        {subscribe, Pid} ->
            #epoch_state{my_up = Up} = State,
            %% check avoid double subscription
            case sets:is_element(Pid, Up) of
                true ->
                    io:format("The PID ~p has already subscribed to me (~p) ~n", [Pid, Self]),
                    epoch_loop(State);
                false ->
                    epoch_loop(State#epoch_state{my_up = sets:add_element(Pid, Up)})
            end;
    end.

%% % @spec ([pid()], Trusted :: sets:set()) -> 
%% %   Leader :: pid() | {error, no_process_trusted}
%% % @doc returns the first trusted process
%% max_rank([], _Trusted) -> {error, no_process_trusted};
%% max_rank([H | T], Trusted) ->
%%     case sets:is_element(H, Trusted) of
%%         true -> H;
%%         false -> max_rank(T, Trusted)
%%     end.
