% link model library

-module(link).
-import(utils, [for_each/2]).
-compile(export_all).

% create a low level link between two nodes.
% The low level link will be registered as 
low_level_link(Node1, Proc_Name1, Pid_Up1, Node2, Proc_Name2, Pid_Up2) ->

% loop of the process of one end of the link
low_level_link_end_loop(Proc_Name, Other_Node, Other_Proc_Name) ->
    

damn_simple_link(Erl_Node1, Erl_Node2) ->
    % register unique random name
    % spawn at node fun() -> register_unique_random_name + send random name & wait for
                % other name
    % receive both names + exchange those names.
    % return both names
damn_simple_link_loop(Other, Up_List) -> 
    receive
        % register the process whishing to receive notifications
        {register, Pid} when is_pid(Pid) ->
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
            
