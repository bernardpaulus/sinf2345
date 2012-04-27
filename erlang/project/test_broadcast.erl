#!/usr/bin/env escript

test() ->
	io:fwrite("Test~n", []),
	broadcast:basic_broadcast().

main(["-t"]) ->
	io:fwrite("Main~n", []),
	test();
main([]) ->
	io:fwrite("Creating broadcasts ~n", []),
	X = broadcast:basic_broadcast(),
	Y = broadcast:basic_broadcast(),
	io:fwrite("Creating links ~n", []),
	{A, B} = link:damn_simple_link(node(), node()),
	X ! {addlink, A},
	Y ! {addlink, B},
	X ! {broadcast, hello}.
