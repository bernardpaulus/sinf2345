-module(broadcast).
-export([
    fib/1,
    basic_broadcast/0,
    basic_broadcast_loop/2
    ]).
-import(lists, [foreach/2]).



basic_broadcast() ->
    Start_loop = fun() ->
        basic_broadcast_loop([], []) 
    end,
    spawn(Start_loop).

basic_broadcast_loop(Us_List, P2P_Links) ->
    io:format("Test1~n", []),
    receive
        % a user subscribe to a broadcast
        % {subscribe, Us_Pid} ->
        %     basic_broadcast_loop([Us_Pid | Us_List], P2P_Links);

        % add a link
        {addlink, P2P_Link} ->
            io:format("addlink ~p", [P2P_Link]),
            basic_broadcast_loop(Us_List, [P2P_Link | P2P_Links]);
        
        % a user send a message to all
        {broadcast, Msg} -> 
            foreach(
                fun(P2P_Link) ->
                    P2P_Link ! {send, Msg},
                    io:format("process ~p send", [P2P_Link])
                end,
                P2P_Links),
            basic_broadcast_loop(Us_List, P2P_Links);
        
        % the broadcast deliver the message to the user
        {deliver, Msg, Us_Pid} ->
            io:format("process ~p received~n~p~n", [Us_Pid, Msg]),
            basic_broadcast_loop(Us_List, P2P_Links)
    end.


