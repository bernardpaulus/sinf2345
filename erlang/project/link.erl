% link model library

-module(link).
-import(lists, [map/2, partition/2, foreach/2]).

% use this
-export([fair_loss_link/2,
         perfect_link/2,
         stubborn_link/2
         ]).

-export([damn_simple_link_register/2,
        damn_simple_link_startup/1,
        damn_simple_link_loop/2,
        fair_loss_link_end/2,
        fair_loss_link_loop/3,
        stubborn_link_end/2,
        stubborn_link_loop/4
        ]).

% TODO refactor

% create a damn_simple_link between two nodes (they can be equals)
%
% this is a wrapper for damn_simple_link_register
%
% @param: Erl_Node1,Erl_Node2 : atoms, names of valid nodes
% @return: [{Name1, Erl_Node1}, {Name2, Erl_Node2}]
damn_simple_link(Erl_Node1, Erl_Node2) ->
    utils:subroutine(?MODULE, damn_simple_link_register, [Erl_Node1, Erl_Node2]).

% create a damn_simple_link between two nodes (they can be equals)
%
% @param: Erl_Node1,Erl_Node2 : atoms, names of valid nodes
% @post: the current process is registered
% @return: [{Name1, Erl_Node1}, {Name2, Erl_Node2}]
damn_simple_link_register(Erl_Node1, Erl_Node2) ->
    Self_name = utils:register_unique(damn_simple_link_spawner, self()),
    % receive both names + exchange those names.
    spawn(Erl_Node1, ?MODULE, damn_simple_link_startup, [{Self_name, node()}]),
    receive {name, Proc1} ->
        spawn(Erl_Node2, ?MODULE, damn_simple_link_startup, [{Self_name, node()}]),
        receive {name, Proc2} ->
            Proc1 ! {other, Proc2},
            Proc2 ! {other, Proc1}
        end
    end,
    % return both names and nodes
    [Proc1, Proc2].

% register the process, wait for the {name, node} of the other side
% then start loop
damn_simple_link_startup(Spawner_process) ->
    Self_name = utils:register_unique(damn_simple_link, self()),
    Spawner_process ! {name, {Self_name, node()}},
    receive {other, Other_proc} -> 
        damn_simple_link_loop(Other_proc, [])
    end.

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


% Fair-loss link in both directions
% @return: [{Fair_loss_name1, Erl_Node1}, {Fair_loss_name2, Erl_Node2}]
fair_loss_link({Name1, Erl_Node1}, {Name2, Erl_Node2}) ->
    utils:subroutine(fun() ->
        Name = utils:register_unique(tmp_name, self()),
        spawn(Erl_Node1, ?MODULE, fair_loss_link_end, 
                    [{Name1, Erl_Node1}, {Name, node()}]),
        receive Pid1 -> Pid1 end,
        spawn(Erl_Node2, ?MODULE, fair_loss_link_end, 
                    [{Name2, Erl_Node2}, {Name, node()}]),
        receive Pid2 -> Pid2 end,
        [Pid1, Pid2]
    end).
    

% One end of a fair loss link
% get a name, give it to spawner, 
fair_loss_link_end(Pid, Spawner_process) ->
    Name = utils:register_unique(fair_loss_link, self()),
    Spawner_process ! {Name, node()}, 
        % send how to access this process to the spawner
    Pid ! {subscribe, {Name, node()}},
    fair_loss_link_loop(Pid, [], []).

% loop when no messages are delayed/reordered
fair_loss_link_loop(Down, Up_List, []) ->
    receive
        {subscribe, Pid} ->
            fair_loss_link_loop(Down, [Pid | Up_List], []);
        % message from upper layer
        {send, Msg} ->
            P = random:uniform(),
            if 
                P < 0.1 -> % Msg loss
                    fair_loss_link_loop(Down, Up_List, []);
                P < 0.2 -> % Msg reordering/delay
                    TTL = random:uniform(5) + 1, % time to live = number of
                        % iterations before sending
                    fair_loss_link_loop(Down, Up_List, 
                                        [{TTL, Msg}]);
                true -> 
                    Down ! {send, Msg},
                    fair_loss_link_loop(Down, Up_List, [])
            end;
        {deliver, Msg} ->
            foreach(fun(Pid) -> Pid ! {deliver, Msg} end, Up_List),
            fair_loss_link_loop(Down, Up_List, [])
    end;

% loop when messages are delayed
% TTL = time to live = number of iterations before sending
% the idea is to reorder/delay messages quickly when messages are exchanged
% quickly
fair_loss_link_loop(Down, Up_List, Old_Buffer) ->
    % decrement all the TTLs
    TTL_Dec = map(fun({TTL, Msg}) -> {TTL - 1, Msg} end, Old_Buffer),
    {TTL_Zero, Buffer} = partition(fun({TTL, _}) ->  TTL == 0 end, TTL_Dec),
    foreach(fun({0, Msg}) -> Down ! {send, Msg} end, TTL_Zero),
    receive
        {subscribe, Pid} ->
            fair_loss_link_loop(Down, [Pid | Up_List], Buffer);
        % message from upper layer
        {send, Msg} ->
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
    after 100 -> % maximum time per iteration
        fair_loss_link_loop(Down, Up_List, Buffer)
    end.

% creates a stubborn link
% @return: [{Stubborn_name1, Erl_Node1}, {Stubborn_name2, Erl_Node2}]
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

