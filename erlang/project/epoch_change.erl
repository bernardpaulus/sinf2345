% @doc Leader-based Epoch Change
% based on algo 5.5 p 219
% @author bernard paulus
% @author martin trigaux

-module(epoch_change).
-compile(export_all).
-import(spawn_utils, [spawn_multiple_on_top/3]).

-record(epoch_state, {
          peers = [],
          down = none,
          beb = none,
          trusted = none,
          p2p_link = none,
          my_up = sets:new(),
          lastts = 0}).

%% @spec (Downs, Bebs, Links) -> epoch_change :: [pid()]
%%   Downs = [monarch_eld :: pid()]
%%   Bebs = [Beb :: pid()]
%%   Links = [Link :: pid()]
%% @doc spawns a epoch_change instance.
start(Downs, Bebs, Links) when
        is_pid(hd(Downs)), is_pid(hd(Bebs)), is_pid(hd(Links)),
        length(Downs) == length(Bebs), length(Bebs) == length(Links) ->
    spawn_multiple_on_top(Downs, [fun init/4 || _ <- Downs],
        [[Beb, Link] || {Beb, Link} <- lists:zip(Bebs, Links)]).
    
start(Nodes) ->
    Links = link:perfect_link(Nodes),
    Bebs = beb:start(Links),
    Fail_Dets = inc_timeout_fd:start(Links),
    Mon_ELD = monarch_eld:start(Fail_Dets, Links),
    start(Mon_ELD, Bebs, Links).

init(Peers, Down, Beb, Link) ->
    Link ! {subscribe, self()},
    Beb ! {subscribe, self()},
    Down ! {subscribe, self()},
    epoch_loop(#epoch_state{peers = Peers, down = Down, p2p_link = Link, beb = Beb, lastts = rank(Peers)}).

epoch_loop(State) ->
    Self= self(),
    receive
        %% add a Leader driven consensus to the list of ups
        {subscribe, Pid} ->
            #epoch_state{my_up = Up} = State,
            %% check avoid double subscription
            % no! we don't give a damn: the net effect is the same
            epoch_loop(State#epoch_state{my_up = sets:add_element(Pid, Up)});

        %% receive a leader election message from meld (Down)
        %% will only match when the Down node is the leader
        {trust, Leader, Leader} ->
            #epoch_state{beb = Beb, lastts = Lastts} = State,
            N = length(State#epoch_state.peers),
            Beb ! {broadcast, {newepoch, Lastts + N}},
            epoch_loop(State#epoch_state{lastts = Lastts + N, trusted = Leader});
        
        %% receive newpoch broadcast message
        {deliver, From, {newepoch, New_Ts}} ->
            #epoch_state{lastts = Lastts, trusted = Trust, down = Down, p2p_link = Link} = State,
            if Trust == From, New_Ts > Lastts ->
                    Down ! {startepoch, New_Ts, Trust},
                    epoch_loop(State#epoch_state{lastts = New_Ts});
               
               true ->
                    Link ! {send, Self, Trust, {nack, Self}},
                    epoch_loop(State)
            end;
        
        %% receive non acknowledgment
        {deliver, From, {nack, _From}} ->
            #epoch_state{lastts = Lastts, trusted = Trust, beb = Beb} = State,
            
            if Trust == Self ->
                    N = length(State#epoch_state.peers),
                    Beb ! {broadcast, {newepoch, Lastts + N}},
                    epoch_loop(State#epoch_state{lastts = Lastts + N, trusted = From});
               true ->
                    pass
            end
        end.


rank(Peers) -> rank(self(), Peers, 1).
rank(_, [], _) -> {error, node_not_found};
rank(Node, [H | T], N) ->
    case H == Node of
        true -> N;
        false -> rank(Node, T, N+1)
    end.
