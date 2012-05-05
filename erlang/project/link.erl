% @author bernard paulus
% @author martin trigaux
% @doc link model library

-module(link).
-import(lists, [partition/2, foreach/2]).
-import(spawn_utils, [
        spawn_same_node/2,
        spawn_multiple/2,
        spawn_multiple_on_top/2
        ]).

% use this
-export([damn_simple_link/1,
        perfect_link/1
         ]).

-export([damn_simple_link_init/1,
        perfect_link_init/2,
        perfect_link_loop/1
        ]).

% @type sets() = erlang_sets
% @type dict() = erlang_dict


% @spec (Nodes::[node()]) -> [pid()]
% @doc spawn a simple_link process on each node, and returns their pids.
% they are 'connected in full mesh'
damn_simple_link(Nodes) when is_list(Nodes) ->
    spawn_multiple(Nodes, [fun damn_simple_link_init/1 || 
            _ <- lists:seq(1, length(Nodes)) ]).

% @type dsl_state() = #dsl_state{
%    others = [pid()], 
%    my_up = sets(), 
%    all_up = dict()}
-record(dsl_state, {
    others = [], 
    my_up = sets:new(), 
    all_up = dict:new()}).

% @spec (Nodes::[node()]) -> term()
% @doc initializes the simple link and never returns.
damn_simple_link_init(Others) ->
    damn_simple_link_loop(#dsl_state{others=Others}).

% @spec (dsl_state()) -> term()
% @doc handle events.
% {subscribe, Up} -> register process in the set of destinations, <br/> 
% the full mesh got a {subscribe, Up}, <br/>
% {send, _From, To, _Msg} -> send a message to To, <br/>
% {deliver, Other, Self, M} -> transmit a message within the full mesh of
% damn_simple_link processes. Should not be used by other processes.
damn_simple_link_loop(State) ->
    Self = self(),
    receive
        {subscribe, Up} = M ->
            #dsl_state{others=Others, my_up = My_Up, all_up=All_Up}
                    = State,
            % transmit the request to all others
            [Other ! {deliver, self(), Other, M} || 
                    Other <- Others, Other /= self()],
            damn_simple_link_loop(State#dsl_state{
                    my_up = sets:add_element(Up, My_Up),
                    all_up = dict:store(Up, self(), All_Up)}); 

        {send, _, To, _}  = M ->
            #dsl_state{all_up = All_Up} = State,
            Other = dict:fetch(To, All_Up), % crash process if not found in dict
            Other ! {deliver, self(), Other, M},
            damn_simple_link_loop(State);

        {deliver, _, Self, {send, From, To, Msg}} -> 
            #dsl_state{my_up=My_Up} = State,
            % translate the "send" in "deliver"
            case sets:is_element(To, My_Up) of true ->
                To ! {deliver, From, To, Msg},
                damn_simple_link_loop(State)
            end;

        {deliver, Other, Self, {subscribe, Up}} -> 
            All_Up = State#dsl_state.all_up,
            damn_simple_link_loop(State#dsl_state{
                    all_up = dict:store(Up, Other, All_Up)})
    end.

% @spec (L) -> [pid()]
%   L = [Down] | [Node]
%   Down = pid()
%   Node = node()
% @doc spawns a perfect link either on top of Down, or on the corresponding
% Node.
% The [Down :: pid()] are assumed to be damn_simple_link processes.<br/>
% The [Node :: node()] are alive erlang nodes. <br/>
% This perfect link performs message reordering. <br/>
% returns the list of pids of the perfect link processes spawned.
perfect_link(Downs) when is_pid(hd(Downs)) ->
    spawn_multiple_on_top(Downs, [fun perfect_link_init/2 || 
            _ <- lists:seq(1, length(Downs))]);

perfect_link(Nodes) when is_atom(hd(Nodes)) ->
    perfect_link(damn_simple_link(Nodes));

perfect_link([]) -> []. % particular case


-define(delay, 100).
-define(max_delay, 2000).

% @type pl_state() = #pl_state{
%    others = [pid()], 
%    down = pid(), 
%    my_up = sets(), 
%    all_up = dict()
%    }
-record(pl_state, {
    others = [], 
    down = none, 
    my_up = sets:new(), 
    all_up = dict:new()
    }).

% @doc starts the perfect link.
% this corresponds to the init event
perfect_link_init(Others, Down) ->
    Down ! {subscribe, self()},
    perfect_link_loop(#pl_state{others = Others, down = Down}).

% @doc loop while receiving events.
% {subscribe, Up},
% {send, From, To, Msg},
% {deliver, From, To, Msg} -> used by the lower levels
perfect_link_loop(State) ->
    Self = self(),
    receive
        {subscribe, Up} = M ->
            #pl_state{down = Down, others=Others, my_up = My_Up, 
                    all_up=All_Up} = State,
            % transmit the request to all others
            [Down ! {send, self(), Other, M} || 
                    Other <- Others, Other /= self()],
            perfect_link_loop(State#pl_state{
                    my_up = sets:add_element(Up, My_Up),
                    all_up = dict:store(Up, self(), All_Up)}); 

        % don't delay subscribes
        {send, _From, To, {subscribe, _}} = M ->
            #pl_state{down = Down, all_up = All_Up} = State,
            Other = dict:fetch(To, All_Up), % crash process if not found in dict
            Down ! {send, self(), Other, M},
            _From ! ok,
            perfect_link_loop(State);

        {send, _, To, _ } = M ->
            #pl_state{down = Down, all_up = All_Up} = State,
            Other = dict:fetch(To, All_Up), % crash process if not found in dict
            Msg = {send, self(), Other, M},
            P = random:uniform(),
            if 
                P < 0.2 -> % Msg reordering
                    erlang:send_after(random:uniform(?max_delay), Down, Msg);
                true -> 
                    % always delay a bit to keep "instant computation" hypothesis
                    erlang:send_after(?delay, Down, Msg)
            end,
            perfect_link_loop(State);
            
        {deliver, _, Self, {send, From, To, Msg}} -> 
            #pl_state{my_up = My_Up} = State,
            % translate the "send" in "deliver"
            case sets:is_element(To, My_Up) of true ->
                To ! {deliver, From, To, Msg},
                perfect_link_loop(State)
            end;

        {deliver, Other, Self, {subscribe, Up}} -> 
            All_Up = State#pl_state.all_up,
            perfect_link_loop(State#pl_state{
                    all_up = dict:store(Up, Other, All_Up)})
    end.

