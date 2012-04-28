% @author bernard paulus
% @author martin trigaux
% @doc link model library

-module(link).
-import(lists, [map/2, partition/2, foreach/2]).

% use this
-export([damn_simple_link/1,
        perfect_link/1
         ]).

-export([damn_simple_link_init/1,
        fair_loss_link_loop/3,
        stubborn_link_loop/4,
        spawn_same_node/2,
        spawn_same_node/3,
        spawn_multiple/2,
        spawn_multiple/3,
        spawn_multiple_on_top/2,
        spawn_multiple_on_top/3,
        perfect_link_init/2,
        perfect_link_loop/1
        ]).

% @type sets() = erlang_sets
% @type dict() = erlang_dict

% @spec (Nodes, Funs, Argss) -> [Pids :: pid()]
%   Nodes = [node()]
%   Funs = [Fun :: function()]
%   Argss = [Args]
%   Args = [term()]
% @doc for each (Node, Fun, Args), spawn Fun on node Node with [Pids | Args] as
% argument.
% Pids is the list of the Pid of all those spawned processes.
spawn_multiple(Nodes, Funs, Argss) when 
        is_list(Nodes), is_list(Funs), is_list(Argss),
        length(Nodes) == length(Funs), length(Funs) == length(Argss) ->
    Parent = self(),
    % acknowledge, then start Fun with the Pids and the args as argument
    Start_Fun = fun(Fun, Args) ->
            fun() -> 
                receive {Parent, Pids} -> 
                    Parent ! {ack, self()},
                    apply(Fun, [Pids | Args])
                end
            end
        end,
    Pids = [spawn(Node, Start_Fun(Fun, Args)) || 
                    {Node, Fun, Args} <- lists:zip3(Nodes, Funs, Argss)],
    % send list of Pids
    [ Pid ! {Parent, Pids} || Pid <- Pids ],
    % ack
    [receive {ack, Pid} -> ok end || Pid <- Pids],
    Pids.

% @spec (Nodes, Funs) -> [Pids :: pid()]
%   Nodes = [node()]
%   Funs = [Fun :: function()]
% @equiv spawn_multiple(Nodes, Funs, [[] || _ <- lists:seq(1, length(Funs))])
spawn_multiple(Nodes, Funs) when is_list(Nodes), is_list(Funs), 
        length(Nodes) == length(Funs) ->
    spawn_multiple(Nodes, Funs, [[] || _ <- lists:seq(1, length(Funs))]).


% @spec (Downs, Funs) -> [Pids :: pid()]
%   Downs = [Down :: pid()]
%   Funs = [Fun :: function()]
% @equiv spawn_multiple_on_top(Downs, Funs, [[] || _ <- lists:seq(1, length(Funs))])
spawn_multiple_on_top(Downs, Funs) when is_list(Downs), is_list(Funs), 
        length(Downs) == length(Funs) ->
    spawn_multiple_on_top(Downs, Funs, [[] || _ <- lists:seq(1, length(Funs))]).


% @spec (Downs, Funs, Argss) -> [Pids :: pid()]
%   Downs = [Down :: pid()]
%   Funs = [Fun :: function()]
%   Argss = [Args]
%   Args = [term()]
% @doc spawn multiple processes, each on top of a process Down.
% spawn each Fun on the same node as the corresponding Down, passing 
% [Others, Down | Args] as argument.
spawn_multiple_on_top(Downs, Funs, Argss) when
        is_list(Downs), is_list(Funs), is_list(Argss),
        length(Downs) == length(Funs), length(Funs) == length(Argss) ->
    Nodes = [node(Down) || Down <- Downs],
    Argss1 = [ [Down | Args] || {Down, Args} <- lists:zip(Downs, Argss)],
    spawn_multiple(Nodes, Funs, Argss1).

% @spec (Down, Fun) -> pid()
%   Down = pid(),
%   Fun = () -> T
% @equiv spawn_same_node(Down, Fun, [])
% @doc spawns Fun on the same node as Down.
spawn_same_node(Down, Fun) when is_function(Fun, 0) ->
    spawn_same_node(Down, Fun, []).

% @spec (Down, Fun, Args) -> pid()
%   Down = pid(),
%   Fun = function(),
%   Args = [term()]
% @doc spawns Fun with the list of arguments Args on the same node as Down.
spawn_same_node(Down, Fun, Args) when is_function(Fun), is_list(Args) ->
    Parent = self(),
    Pid = spawn(node(Down), fun() ->
            Parent ! {ack, self()},
            apply(Fun, Args)
        end),
    % wait for ack
    receive {ack, Pid} -> ok end,
    Pid.

% @spec (Nodes::[node()]) -> [pid()]
% @doc spawn a simple_link process on each node, and returns their pids.
% they are 'connected in full mesh'
damn_simple_link(Nodes) when is_list(Nodes) ->
    spawn_multiple(Nodes, [fun damn_simple_link_init/1 || 
            _ <- lists:seq(1, length(Nodes)) ]).

-record(dsl_state, {
    others = [], 
    my_up = sets:new(), 
    all_up = dict:new(),
    seq = 0}).

% @spec (Nodes::[node()]) -> term()
% @doc initializes the simple link and never returns.
damn_simple_link_init(Others) ->
    damn_simple_link_loop(#dsl_state{others=Others}).

% @doc handle events.
% {subscribe, Up, _Seq} -> register process in the set of destinations, <br/> 
% the full mesh got a {subscribe, Up, _Seq}, <br/>
% {send, _From, To, _Seq, _Msg} -> send a message to To, <br/>
% {deliver, Other, Self, _Seq, M} -> transmit a message within the full mesh of
% damn_simple_link processes. Should not be used by other processes.
damn_simple_link_loop(State) ->
    Self = self(),
    receive
        {subscribe, Up, _} = M ->
            #dsl_state{others=Others, seq=Seq_Num, my_up = My_Up, all_up=All_Up}
                    = State,
            % transmit the request to all others
            [Other ! {deliver, self(), Other, S, M} || 
                    {Other, S} <- lists:zip(Others, lists:seq(Seq_Num, Seq_Num +
                    length(Others) - 1)), Other /= self()],
            damn_simple_link_loop(State#dsl_state{
                    my_up = sets:add_element(Up, My_Up),
                    all_up = dict:append(Up, self(), All_Up), 
                    seq = Seq_Num + length(Others)}); 
        {send, _, To, _, _}  = M ->
            #dsl_state{seq = Seq_Num, all_up = All_Up} = State,
            [Other] = dict:fetch(To, All_Up), % crash process if not found in dict
            Other ! {deliver, self(), Other, Seq_Num, M},
            damn_simple_link_loop(State#dsl_state{
                    seq =  Seq_Num + 1});
        {deliver, _, Self, _, {send, From, To, S, Msg}} -> 
            #dsl_state{my_up=My_Up} = State,
            % translate the "send" in "deliver"
            case sets:is_element(To, My_Up) of true ->
                To ! {deliver, From, To, S, Msg},
                damn_simple_link_loop(State)
            end;
        {deliver, Other, Self, _, {subscribe, Up, _}} -> 
            All_Up = State#dsl_state.all_up,
            damn_simple_link_loop(State#dsl_state{
                    all_up = dict:append(Up, Other, All_Up)})
    end.

perfect_link(Downs) when is_list(Downs) ->
    spawn_multiple_on_top(Downs, [fun perfect_link_init/2 || 
            _ <- lists:seq(1, length(Downs))]).

% @type pl_state() = #pl_state{
%    others = [pid()], 
%    down = pid(), 
%    my_up = sets(), 
%    all_up = dict(),
%    seq = integer(),
%    buffer = list(),
%    delta = integer(), % ms
%    ttl = integer() % number of iterations
%    }
-record(pl_state, {
    others = [], 
    down = none, 
    my_up = sets:new(), 
    all_up = dict:new(),
    seq = 1,
    buffer = [],
    delta = 100, % ms
    ttl = 64 % number of iterations
    }).

perfect_link_init(Others, Down) ->
    Down ! {subscribe, self(), 0},
    perfect_link_loop(#pl_state{others = Others, down = Down}).

perfect_link_loop(State0) ->
    % loop each 100 ms when messages are delayed/reordered, upon message reception
    % otherwise
    %
    % TTL = time to live = number of iterations before sending
    % the idea is to reorder/delay messages quickly when messages are exchanged
    % quickly
    State = perfect_link_loop_decrement_ttl(State0),
    Self = self(),
    Delta = State#pl_state.delta,
    receive
        {subscribe, Up, _} = M ->
            #pl_state{down = Down, others=Others, seq=Seq_Num, my_up = My_Up, 
                    all_up=All_Up} = State,
            % transmit the request to all others
            [Down ! {send, self(), Other, S, M} || 
                    {Other, S} <- lists:zip(Others, lists:seq(Seq_Num, Seq_Num +
                    length(Others) - 1)), Other /= self()],
            perfect_link_loop(State#pl_state{
                    my_up = sets:add_element(Up, My_Up),
                    all_up = dict:append(Up, self(), All_Up), 
                    seq = Seq_Num + length(Others)}); 
        {send, _, To, _, _}  = M ->
            #pl_state{down = Down, seq = Seq_Num, all_up = All_Up, ttl =
                Max_TTL, buffer = Buffer} = State,
            [Other] = dict:fetch(To, All_Up), % crash process if not found in dict
            Msg = {send, self(), Other, Seq_Num, M},
            P = random:uniform(),
            if 
                P < 0.2 -> % Msg reordering
                    TTL = random:uniform(Max_TTL) + 1, % number of iterations
                    perfect_link_loop(State#pl_state{
                            seq =  Seq_Num + 1,
                            buffer = [{TTL, Msg} | Buffer]});
                true -> 
                    Down ! Msg,
                    perfect_link_loop(State#pl_state{
                            seq =  Seq_Num + 1})
            end;
        {deliver, _, Self, _, {send, From, To, S, Msg}} -> 
            #pl_state{my_up = My_Up} = State,
            % translate the "send" in "deliver"
            case sets:is_element(To, My_Up) of true ->
                To ! {deliver, From, To, S, Msg},
                perfect_link_loop(State)
            end;
        {deliver, Other, Self, _, {subscribe, Up, _}} -> 
            All_Up = State#pl_state.all_up,
            perfect_link_loop(State#pl_state{
                    all_up = dict:append(Up, Other, All_Up)})
    after Delta ->
        perfect_link_loop(State)
    end.

% @spec (pl_state()) -> pl_state()
% @doc decrements all the ttls present in the buffer, and sends the ones that reached
% zero.
perfect_link_loop_decrement_ttl(State) ->
    #pl_state{down = Down, buffer = Old_Buffer} = State,
    TTL_Dec = [ {TTL - 1, Msg} || {TTL, Msg} <- Old_Buffer],
    {TTL_Zero, Buffer} = partition(fun({TTL, _}) ->  TTL == 0 end, TTL_Dec),
    foreach(fun({0, Msg}) -> Down ! Msg end, TTL_Zero),
    State#pl_state{buffer = Buffer}.
