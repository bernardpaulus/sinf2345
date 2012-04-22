% link model library

-module(link)
-compile(export_all)

% create a low level link between two nodes.
% The low level link will be registered as 
low_level_link(Node1, Proc_Name1, Pid_Up1, Node2, Proc_Name2, Pid_Up2) ->

low_level_link_end(Pid_up, Other_Node, 
