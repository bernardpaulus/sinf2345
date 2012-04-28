% @author bernard paulus
% @author martin trigaux
% @doc link model library

-module(link).
-import(lists, [map/2, partition/2, foreach/2]).

% use this
-export([damn_simple_link/1
         ]).

-export([damn_simple_link_init/1,
        damn_simple_link_loop/4,
        fair_loss_link_loop/3,
        stubborn_link_loop/4,
        spawn_same_node/2,
        spawn_same_node/3,
        spawn_multiple/2,
        spawn_multiple/3
        ]).

% @spec (Nodes, Funs, Argss) -> [Pids :: pid()]
%   Nodes = [node()]
%   Funs = [Fun]
%   Fun = function()
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
%   Funs = [Fun]
%   Fun = function()
% @equiv spawn_multiple(Nodes, Funs, [[] || _ <- lists:seq(1, length(Funs))])
spawn_multiple(Nodes, Funs) when is_list(Nodes), is_list(Funs), 
        length(Nodes) == length(Funs) ->
    spawn_multiple(Nodes, Funs, [[] || _ <- lists:seq(1, length(Funs))]).


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

damn_simple_link(Nodes) when is_list(Nodes) ->
    spawn_multiple(Nodes, [fun damn_simple_link_init/1 || 
            _ <- lists:seq(1, length(Nodes)) ]).

damn_simple_link_init(Others) ->
    damn_simple_link_loop(Others, sets:new(), dict:new(), 0).

damn_simple_link_loop(Others, My_Up, All_Up, Seq_Num) ->
    Self = self(),
    receive
        % register the process whishing to receive notifications
        {subscribe, Up, _Seq} ->
            [Other ! {subscribe, Up, at, self(), Seq_Num} || 
                    Other <- Others, Other /= self()],
            damn_simple_link_loop(Others, sets:add_element(Up, My_Up),
                    dict:append(Up, self(), All_Up), Seq_Num + 1); 
        % receive notification of subscription on another node
        {subscribe, Up, at, Other, _Seq} ->
            damn_simple_link_loop(Others, My_Up, dict:append(Up, Other, All_Up),
                    Seq_Num);
        % receive a message from the upper layer
        {send, _From, To, _Seq, _Msg}  = M ->
            [Other] = dict:fetch(To, All_Up), % crash process if not found in dict
            Other ! {transmit, self(), Other, Seq_Num, M},
            damn_simple_link_loop(Others, My_Up, All_Up, Seq_Num + 1);
        % receive a message from the other end of the channel
        {transmit, Other, Self, _Seq, {send, From, To, Seq, Msg}} -> 
            case sets:is_element(To, My_Up) of true ->
                To ! {deliver, From, To, Seq, Msg},
                damn_simple_link_loop(Other, My_Up, All_Up, Seq_Num)
            end
    end.


% loop each 100 ms when messages are delayed/reordered, upon message reception
% otherwise
%
% TTL = time to live = number of iterations before sending
% the idea is to reorder/delay messages quickly when messages are exchanged
% quickly
fair_loss_link_loop(Down, Up_List, Old_Buffer) ->
    % decrement all the TTLs
    TTL_Dec = map(fun({TTL, Msg}) -> {TTL - 1, Msg} end, Old_Buffer),
    {TTL_Zero, Buffer} = partition(fun({TTL, _}) ->  TTL == 0 end, TTL_Dec),
    foreach(fun({0, Msg}) -> Down ! {send, Msg} end, TTL_Zero),
    % change the maximum delay before automatic next iteration.
    case Buffer of
        [] -> Loop_after = infinity;
        _ -> Loop_after = 100
    end,
    receive
        % upper layer subscribes to {deliver, } messages
        {subscribe, Pid} ->
            fair_loss_link_loop(Down, [Pid | Up_List], Buffer);
        % message from upper layer
        {send, Msg} ->
            io:format("~p: ~p~n", [self(), {self(), Msg}]),
            P = random:uniform(),
            if 
                P < 0.1 -> % Msg loss
                    fair_loss_link_loop(Down, Up_List, Buffer);
                P < 0.2 -> % Msg reordering
                    TTL = random:uniform(10) + 1, % number of iterations
                    fair_loss_link_loop(Down, Up_List, 
                                        [{TTL, Msg} | Buffer]);
                true -> 
                    Down ! {send, Msg},
                    fair_loss_link_loop(Down, Up_List, Buffer)
            end;
        {deliver, Msg} ->
            [Pid ! {deliver, Msg} || Pid <- Up_List],
            fair_loss_link_loop(Down, Up_List, Buffer)
    after Loop_after -> 
        fair_loss_link_loop(Down, Up_List, Buffer)
    end.


stubborn_link_loop(Down, Up_List, Delta, Sent) ->
    receive
        % register interested processes
        {subscribe, Pid} ->
            stubborn_link_loop(Down, [Pid | Up_List], Delta, Sent);
        % periodic resent
        {timeout} ->
            [ Down ! {send, Msg} || Msg <- sets:to_list(Sent)],
            erlang:send_after(Delta, self(), {timeout}),
            stubborn_link_loop(Down, Up_List, Delta, Sent);
        % upper layer request
        {send, Msg} ->
            Down ! {send, Msg},
            stubborn_link_loop(Down, Up_List, Delta, 
                                sets:add_element(Msg, Sent));
        % receive a message from Down
        {deliver, Msg} ->
            [Pid ! {deliver, Msg} || Pid <- Up_List],
            stubborn_link_loop(Down, Up_List, Delta, Sent)
    end.

