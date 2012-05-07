% @doc Monarchical Eventual Leader Detector
% based on algo 2.8 p 57
% @author bernard paulus
% @author martin trigaux

-module(monarch_eld).
-compile(export_all).
-import(spawn_utils, [spawn_multiple_on_top/3]).

-record(eld_state, {
          peers = [],
          my_up = sets:new(),
          ups_of_others = dict:new(),
          suspected = sets:new(),
          leader = none,
          p2p_link = none}). % p2p links to up nodes

start(Fail_Dets, Perfect_Links) when 
      is_pid(hd(Fail_Dets)), is_pid(hd(Perfect_Links)),
      length(Fail_Dets) == length(Perfect_Links) ->
                    spawn_multiple_on_top(Fail_Dets, 
                                          [fun init/3 || _ <- lists:seq(1,length(Fail_Dets))],
                                          [[Link] || Link <- Perfect_Links] ).

init(Peers, FD, Link) ->
    Link ! {subscribe, self()},
    FD ! {subscribe, self()},
    % condition
    Self = self(),
    Peers_set = sets:from_list(Peers),
    condition:start(),
    condition:upon(
        fun (#eld_state{leader = Leader, suspected = Suspected}) ->
            Trusted = sets:subtract(Peers_set, Suspected),
            case max_rank(Peers, Trusted) of
                Leader -> ok;
                New_Leader -> Self ! {leader_not_trusted, New_Leader}
            end,
            false % trick the condition module into believing condition is
                  % never satisfied
        end,
        fun (_) -> dummy_action end),
    meld_loop(#eld_state{peers = Peers, p2p_link = Link}).

meld_loop(State) ->
    Self= self(),
    condition:check(State),
    receive
        % add a process to the eligeable list
        {subscribe, Up} = M ->
            #eld_state{my_up = My_Up, peers = Peers, ups_of_others = O_Ups,
                p2p_link = Link} = State,
            link(Up),
            [Link ! {send, self(), Other, M} || 
                Other <- Peers, Other /= self()],
            meld_loop(State#eld_state{
                my_up = sets:add_element(Up, My_Up),
                ups_of_others = dict:append(self(), Up, O_Ups)}); 


        % receive subscription on another meld
        {deliver, Other, Self, {subscribe, Up}} -> 
            O_Ups = State#eld_state.ups_of_others,
            meld_loop(State#eld_state{
                    ups_of_others = dict:append(Other, Up, O_Ups)});
    
    
        % add a pid and its subscribers to the list of considered dead
        {suspect, _From, Subscribers} ->
            % case sets:is_element(Leader, Subscribers) of % Leader == bottom !
            State1 = State#eld_state{
                        suspected = sets:union(Subscribers, 
                             State#eld_state.suspected)},
            meld_loop(State1);

    
        % oups it seems you are not dead after all, welcome back
        {restore, _From, Subscribers} ->
            #eld_state{suspected = Suspected} = State,
            State1 = State#eld_state{
                        suspected = sets:subtract(Suspected, Subscribers)},
            meld_loop(State1);
    

        % leader /= max_rank(Peers \ suspected)
        {leader_not_trusted, New_Leader} -> 
            L = State#eld_state.leader,
            case New_Leader of
                L ->
                    % nothing to do
                    meld_loop(State);
                _ ->
                    #eld_state{my_up = My_Up, ups_of_others = O_Ups} = State,
                    case dict:find(New_Leader, O_Ups) of
                        {ok, Leader_Ups} -> Leader_Ups;
                        error -> Leader_Ups = []
                    end,
                    %io:format("ld ~p trust ~p, ~p~n", 
                    %        [Self, New_Leader, Leader_Ups]),
                    [ Up ! {trust, Self, New_Leader, {ups, Leader_Ups}}
                            || Up <- sets:to_list(My_Up) ],
                    State1 = State#eld_state{
                                leader = New_Leader},
                    condition:check(State1),
                    meld_loop(State1)
            end;
            
        % forcibly reads the reader => initialize reader in epoch_change
        {force_trust, From} ->
            #eld_state{ups_of_others = O_Ups, leader = Leader} = State,
            case dict:find(Leader, O_Ups) of
                {ok, Leader_Ups} -> Leader_Ups;
                error -> Leader_Ups = []
            end,
            From ! {trust, Self, Leader, {ups, Leader_Ups}},
            meld_loop(State)

    end.

% @spec (Peers, LD) -> Leader :: pid() | Multiple :: {multiple, [pid()]}
%   Peers = [pid()]
%   LD = pid()
% @doc asks for a leader in the list of Peers and returns it.
% When multiple leaders are found Multiple is returned
wait_for_trust(Peers, LD) ->
    LD ! {force_trust, self()},
    receive after 1000 -> wait end,
    receive 
        {trust, _Self, _New_Leader, {ups, Leader_Ups}} ->
            Leader_Ups
    end,
    Leaders = sets:to_list(sets:intersection(
                    sets:from_list(Peers),
                    sets:from_list(Leader_Ups))),
    case Leaders of
        [] ->
            wait_for_trust(Peers, LD);
        [Leader] ->
            Leader;
        Ls when is_list(Ls) ->
            {multiple, Ls}
    end.

% @spec ([pid()], Trusted :: sets:set()) -> 
%   Leader :: pid() | {error, no_process_trusted}
% @doc returns the first trusted process
max_rank([], _Trusted) -> {error, no_process_trusted};
max_rank([H | T], Trusted) ->
    case sets:is_element(H, Trusted) of
        true -> H;
        false -> max_rank(T, Trusted)
    end.
