% @doc Leader-based Epoch Change
% based on algo 5.5 p 219
% @author bernard paulus
% @author martin trigaux

-module(epoch).
-compile(export_all).
-import(spawn_utils, [spawn_multiple_on_top/3]).

-record(epoch_state, {
          peers = [],
          down = none,
          beb = none,
          trusted = none,
          p2p_link = none,
          my_up = sets:new(),
          lastts = 0}). % p2p links to up nodes

%% start(Fail_Dets, Perfect_Links) when 
%%       is_pid(hd(Fail_Dets)), is_pid(hd(Perfect_Links)),
%%       length(Fail_Dets) == length(Perfect_Links) ->
%%                     spawn_multiple_on_top(Fail_Dets, 
%%                                           [fun init/3 || _ <- lists:seq(1,length(Fail_Dets))],
%%                                           [[Link] || Link <- Perfect_Links] ).

init(Peers, Down, Beb, Link) ->
    Link ! {subscribe, self()},
    Beb ! {subscribe, self()},
    Down ! {subscribe, self()},
    epoch_loop(#epoch_state{peers = Peers, down = Down, p2p_link = Link, beb = Beb, lastts = 0}).

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

        %% receive a leader election message from meld (Down)
        %% will only match when the Down node is the leader
        {trust, Leader, Leader} ->
            #epoch_state{beb = Beb, lastts = Lastts} = State,
            N = length(State#epoch_state.peers),
            Beb ! {broadcast, {newepoch, Lastts + N}},
            epoch_loop(State#epoch_state{lastts = Lastts + N, trusted = Leader});
        
        %% receive newpoch broadcast message
        {deliver, From, M} ->
            #epoch_state{lastts = Lastts, trusted = Trust, down = Down, p2p_link = Link, beb = Beb} = State,
            {Action, Body} = M,
            if Action == newepoch ->
                    if Trust == From, Body > Lastts ->
                            Down ! {startepoch, Body, Trust},
                            epoch_loop(State#epoch_state{lastts = Body});
                       
                       true ->
                            Link ! {send, Self, Trust, {nack, Self}},
                            epoch_loop(State)
                    end;
               Action == nack ->
                    if Trust == Self ->
                            N = length(State#epoch_state.peers),
                            Beb ! {broadcast, {newepoch, Lastts + N}},
                            epoch_loop(State#epoch_state{lastts = Lastts + N, trusted = From});
                       true ->
                            pass
                    end
            end
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