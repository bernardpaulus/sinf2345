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
          ts = ts,
          n = n,
          lastts = 0}).

%% @spec (Downs, Bebs, Links) -> epoch_change :: [pid()]
%%   Downs = [monarch_eld :: pid()]
%%   Bebs = [Beb :: pid()]
%%   Links = [Link :: pid()]
%% @doc spawns a epoch_change instance.
start(Leader_Dets, Bebs, Links) when
        is_pid(hd(Leader_Dets)), is_pid(hd(Bebs)), is_pid(hd(Links)),
        length(Leader_Dets) == length(Bebs), length(Bebs) == length(Links) ->
    spawn_multiple_on_top(Leader_Dets, [fun init/4 || _ <- Leader_Dets],
        [[Beb, Link] || {Beb, Link} <- lists:zip(Bebs, Links)]).
    
start(Nodes) ->
    Links = link:perfect_link(Nodes),
    Bebs = beb:start(Links),
    Fail_Dets = inc_timeout_fd:start(Links),
    Mon_ELD = monarch_eld:start(Fail_Dets, Links),
    start(Mon_ELD, Bebs, Links).

init(Peers, Leader_Det, Beb, Link) ->
    Link ! {subscribe, self()},
    Beb ! {subscribe, self()},
    Leader_Det ! {subscribe, self()},
    receive after 1000 -> true end, % DEBUG
    % get a leader
    case monarch_eld:wait_for_trust(Peers, Leader_Det) of
        Leader when is_pid(Leader) -> Leader % crash otherwise
    end,
    % and additionnally force to get a {trust ..}
    Leader_Det ! {force_trust, self()},
    epoch_loop(#epoch_state{peers = Peers, down = Leader_Det, p2p_link = Link,
            beb = Beb, lastts = 0, trusted = Leader, ts = rank(Peers),
            n = length(Peers)}).

epoch_loop(State) ->
    Self= self(),
    receive
        %% add a Leader driven consensus to the list of ups
        {subscribe, Pid} ->
            #epoch_state{my_up = Up} = State,
            epoch_loop(State#epoch_state{my_up = sets:add_element(Pid, Up)});

        %% receive a leader election message from meld (Down)
        %% will only match when the Down node is the leader
        {trust, Leader, Leader, {ups, _Epoch_Changes}} ->
            #epoch_state{beb = Beb, ts = Ts, n = N} = State,
            % io:format("~p trusts self()~n", [self()]),
            Beb ! {broadcast, Self, {newepoch, Ts + N}},
            epoch_loop(State#epoch_state{ts = Ts + N, trusted = Self});

        {trust, _From, _Leader, {ups, LD_Ups}} ->
            #epoch_state{peers = Peers} = State,
            % eliminate non-peer processes
            EC_Leaders = sets:intersection(
                                    sets:from_list(Peers),
                                    sets:from_list(LD_Ups)),
            case sets:to_list(EC_Leaders) of
                [EC_Leader] ->
                    % io:format("~p trusts ~p~n", [self(), EC_Leader]),
                    epoch_loop(State#epoch_state{trusted = EC_Leader});
                _Leaders -> 
                    % io:format("~p trusts none: received ~p~n", [self(), _Leaders]),
                    epoch_loop(State#epoch_state{trusted = none})
            end;
        
        %% receive newpoch broadcast message
        {deliver, From, {newepoch, New_Ts}} ->
            #epoch_state{lastts = Lastts, trusted = Trust, my_up = My_Up,
                p2p_link = Link} = State,
            if Trust == From, New_Ts > Lastts ->
                    io:format("~p start epoch ~p ~p~n", [Self, New_Ts, Trust]),
                    [Up ! {startepoch, New_Ts, Trust}
                        || Up <- sets:to_list(My_Up)],
                    epoch_loop(State#epoch_state{lastts = New_Ts});
               
               true ->
                    Link ! {send, Self, From, {nack, Self}},
                    epoch_loop(State)
            end;
        
        %% receive non acknowledgment
        {deliver, _From, Self, {nack, _From}} ->
            #epoch_state{ts = Ts, trusted = Trust, beb = Beb, n = N} = State,
            if Trust == Self ->
                    Beb ! {broadcast, Self, {newepoch, Ts + N}},
                    epoch_loop(State#epoch_state{ts = Ts + N});
               true ->
                    epoch_loop(State)
            end
        end.


rank(Peers) -> rank(self(), Peers, 1).
rank(_, [], _) -> {error, node_not_found};
rank(Node, [H | T], N) ->
    case H == Node of
        true -> N;
        false -> rank(Node, T, N+1)
    end.
