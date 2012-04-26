#!/usr/bin/env escript

% c(test),
% c(link),
% c(utils),

main(_) ->
	X = test:basic_broadcast(),
	Y = test:basic_broadcast(),
	{A, B} = link:damn_simple_link(node(), node()),
	X ! {addlink, A},
	Y ! {addlink, B},
	X ! {broadcast, hello}.
