% link model library

-module(link).
-import(utils, [for_each/2]).
-compile(export_all).

% create a low level link between two nodes.
% The low level link will be registered as 
low_level_link(Node1, Proc_Name1, Pid_Up1, Node2, Proc_Name2, Pid_Up2) ->

% loop of the process of one end of the link
low_level_link_end_loop(Proc_Name, Other_Node, Other_Proc_Name) ->
    

% creates a damn_simple_link between two nodes (they can be equals)
% @post: the current process has a name
% @return: [{Name1, Erl_Node1}, {Name2, Erl_Node2}]
damn_simple_link(Erl_Node1, Erl_Node2) ->
    Self_name = register_unique(damn_simple_link_spawner, self()),
    % receive both names + exchange those names.
    spawn(Erl_Node1, link, damn_simple_link_startup, [{Self_name, node()}]),
    receive {name, Proc1} ->
        spawn(Erl_Node2, link, damn_simple_link_startup, [{Self_name, node()}]),
        receive {name, Proc2} ->
            Proc1 ! {other, Proc2},
            Proc2 ! {other, Proc1}
        end
    end,
    % return both names
    [Other1, Other2].

% register the process, wait for the {name, node} of the other side
% then start loop
damn_simple_link_startup(Spawner_process) ->
    Self_name = register_unique(damn_simple_link, self()),
    Spawner_process ! {name, {Self_name, node()}},
    receive {other, Other_proc} -> 
        damn_simple_link_loop(Other_proc, [])
    end.

damn_simple_link_loop(Other, Up_List) -> 
    receive
        % register the process whishing to receive notifications
        {register, Pid} ->
            damn_simple_link_loop(Other, [Pid | Up_List]);
        % receive a message from the upper layer
        {send, Msg} ->
            Other ! {transmit, Msg},
            damn_simple_link_loop(Other, Up_List);
        % receive a message from the other end of the channel
        {transmit, Msg} ->
            for_each(Up_List, fun(Pid) -> Pid ! {deliver, Msg} end),
            damn_simple_link_loop(Other, Up_List)
    end.
            
