% link model library

-module(link).
-import(lists, [map/2, partition/2, foreach/2]).

% use this
-export([fair_loss_link/2,
         perfect_link/2,
         stubborn_link/2,
         damn_simple_link/2
         ]).

-export([damn_simple_link_loop/2,
        fair_loss_link_loop/3,
        stubborn_link_end/2,
        stubborn_link_loop/4,
        spawn_same_node/2,
        spawn_same_node/3
        ]).

% TODO refactor

% create a damn_simple_link between two nodes (they can be equals)
%
% @param: Erl_Node1,Erl_Node2 : atoms, names of valid nodes
% @return: {Pid1, Pid2}
damn_simple_link(Erl_Node1, Erl_Node2) ->
    Start_loop = fun(Other) -> 
            damn_simple_link_loop(Other, []) 
        end,
    spawn_pair(Erl_Node1, Erl_Node2, Start_loop, Start_loop).


% @spec (Erl_Node1, Erl_Node2, Fun1, Fun2) -> {Pid1::pid(), Pid2::pid()}
%   Erl_Node1 = atom(),
%   Erl_Node2 = atom(),
%   Fun1 = (Pid2::pid()) -> T,
%   Fun2 = (Pid1::pid()) -> T
% @doc spawns a pair of processes, Fun1 on node1, Fun2 on node2
% and returns their two pid, in the same order
spawn_pair(Erl_Node1, Erl_Node2, Fun1, Fun2) ->
    Parent = self(),
    % acknowledge, then start Fun1 or Fun2 according to parameter
    Ack = fun(Fun) ->
            fun() -> 
                receive {Parent, Other} -> 
                    Parent ! {ack, self()},
                    Fun(Other)
                end
            end
        end,
    Pid1 = spawn(Erl_Node1, Ack(Fun1)),
    Pid2 = spawn(Erl_Node2, Ack(Fun2)),
    % transfer the Pid of the other node
    Pid1 ! {self(), Pid2},
    Pid2 ! {self(), Pid1},
    % wait for ack
    receive {ack, Pid1} -> ok end,
    receive {ack, Pid2} -> ok end,
    {Pid1, Pid2}.


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
            % execute function with args
            apply(Fun, Args)
        end),
    % wait for ack
    receive {ack, Pid} -> ok end,
    Pid.


damn_simple_link_loop(Other, Up_List) -> 
    receive
        % register the process whishing to receive notifications
        {subscribe, Pid} ->
            damn_simple_link_loop(Other, [Pid | Up_List]);
        % receive a message from the upper layer
        {send, Msg} ->
            Other ! {transmit, Msg},
            damn_simple_link_loop(Other, Up_List);
        % receive a message from the other end of the channel
        {transmit, Msg} ->
            foreach(fun(Pid) -> Pid ! {deliver, Msg} end, Up_List),
            damn_simple_link_loop(Other, Up_List)
    end.


% Perfect link
perfect_link(Erl_Node1, Erl_Node2) -> damn_simple_link(Erl_Node1, Erl_Node2).

% 
fair_loss_link(Down1, Down2) when is_pid(Down1), is_pid(Down2) ->
    % spawned process subscribes to deliver and then start its loop
    Start_loop = fun(Down) ->
                Down ! {subscribe, self()},
                fair_loss_link_loop(Down, [], [])
        end,
    Pid1 = spawn_same_node(Down1, Start_loop, [Down1]),
    Pid2 = spawn_same_node(Down2, Start_loop, [Down2]),
    {Pid1, Pid2}.


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
            foreach(fun(Pid) -> Pid ! {deliver, Msg} end, Up_List),
            fair_loss_link_loop(Down, Up_List, Buffer)
    after Loop_after -> 
        fair_loss_link_loop(Down, Up_List, Buffer)
    end.


% creates a stubborn link
% @return: [{Stubborn_name1, Erl_Node1}, {Stubborn_name2, Erl_Node2}]
% TODO Pid
stubborn_link({Name1, Erl_Node1}, {Name2, Erl_Node2}) ->
    utils:subroutine(fun() ->
        Name = utils:register_unique(tmp_name, self()),
        spawn(Erl_Node1, ?MODULE, stubborn_link_end, 
                    [{Name1, Erl_Node1}, {Name, node()}]),
        receive Pid1 -> Pid1 end,
        spawn(Erl_Node2, ?MODULE, stubborn_link_end, 
                    [{Name2, Erl_Node2}, {Name, node()}]),
        receive Pid2 -> Pid2 end,
        [Pid1, Pid2]
    end).

% stubborn_link init
stubborn_link_end(Pid, Spawner_process) ->
    % send how to access this process to the spawner
    Name = utils:register_unique(stubborn_link, self()),
    Spawner_process ! {Name, node()}, 
    % ask for deliver notifications
    Pid ! {subscribe, {Name, node()}},
    % start timer
    Delta = 100,
    erlang:send_after(Delta, self(), {timeout}),
    stubborn_link_loop(Pid, [], Delta, sets:new()).

stubborn_link_loop(Down, Up_List, Delta, Sent) ->
    receive
        % register interested processes
        {subscribe, Pid} ->
            stubborn_link_loop(Down, [Pid | Up_List], Delta, Sent);
        % periodic resent
        {timeout} ->
            foreach(fun(Msg) -> Down ! {send, Msg} end, sets:to_list(Sent)),
            erlang:send_after(Delta, self(), {timeout}),
            stubborn_link_loop(Down, Up_List, Delta, Sent);
        % upper layer request
        {send, Msg} ->
            Down ! {send, Msg},
            stubborn_link_loop(Down, Up_List, Delta, 
                                sets:add_element(Msg, Sent));
        % receive a message from Down
        {deliver, Msg} ->
            foreach(fun(Pid) -> Pid ! {deliver, Msg} end, Up_List),
            stubborn_link_loop(Down, Up_List, Delta, Sent)
    end.

