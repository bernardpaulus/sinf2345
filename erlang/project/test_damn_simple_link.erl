#!/usr/bin/env escript
% %%! -sname meincake  % this seems to forbid printing from another process

main(["-h"]) -> io:format(
    "USAGE: test_damn_simple_link.erl~n"
    "   or: test_damn_simple_link.erl NODE1 NODE2~n"); 
main([]) -> 
    [A, B] = link:damn_simple_link(node(), node()),
    Pid_Recv = utils:dummy_receiver(),
    B ! {register, Pid_Recv},
    A ! {send, "you and me and the bottle makes three tonight :)"};
main([Node1, Node2]) ->
    [A, B] = link:damn_simple_link(list_to_atom(Node1), list_to_atom(Node2)),
    register(dummy_receiver, utils:dummy_receiver()),
    B ! {register, {dummy_receiver, node()}},
    A ! {send, "it works within erlang shell!"};
main(_) -> main([-h]).
