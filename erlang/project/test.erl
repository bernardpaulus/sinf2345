

-module(test).
-compile(export_all).


usage() ->
    io:format("Usage: erl -s test main~n"),    
    io:format("           Start the bank with 3 nodes~n"),
    halt(1).

main(N) ->    
    case is_integer(N) of
        false ->
            usage();
        true ->
            Nodes = [node() || _ <- lists:seq(1,N)],
            launch(Nodes)
    end.

main() ->
    launch([node(), node(), node()]).

launch(Nodes) ->
    io:format("~nLaunching sub-modules~n"),

    %% P2P links
    Links = link:perfect_link(Nodes),
    receive after 100 -> pass end,
    %% Best effort broadcasts
    Bebs = beb:start(Links),
    receive after 100 -> pass end,
    %% Eager reliable broadcast
    RBs = erb:start(Bebs),
    receive after 100 -> pass end,
    %% The failure detectors
    FDs = inc_timeout_fd:start(Links),
    receive after 100 -> pass end,
    %% Down, leader detectors
    LDs = monarch_eld:start(FDs, Links),
    receive after 100 -> pass end,
    %% The read write epoch consensus
    RW_Epochs_Cons = rw_epoch_cons:start(Bebs, Links, 1, [{1, bottom} || _ <- Bebs]),
    receive after 100 -> pass end,
    %% The epoch change
    Epoch_Changes = epoch_change:start(LDs, Bebs, Links),
    receive after 100 -> pass end,
    %% The Consensus
    Consensuss = ld_cons:start(RW_Epochs_Cons, Epoch_Changes),
    receive after 100 -> pass end,
    %% The total order boradcast
    TOBs = tob:start(RBs, Consensuss),
    
    receive after 200 -> pass end,
    io:format("~n~nBanking application with ~p nodes~n~n",[length(Nodes)]),
    receive after 200 -> pass end,
    [H | T] = bank:start(TOBs),
    
    io:format("Creating nodes 1 and 2"),
    receive after 200 -> pass end,
    H ! {create, 1, 10},
    
    receive after 300 -> pass end,
    H ! {create, 2, 20},
    
    io:format("Transfering money from #1 to #2"),
    receive after 500 -> pass end,
    H ! {transfer, 1, 2, 5},
    [H | T].



string_to_int(String) ->
    try
        list_to_integer(String)
    catch
        _:_ ->
            false
    end.



ldc() ->
    %% The links
    Links = link:perfect_link([node(), node(), node()]),
    receive after 100 -> pass end,
    %% The failure detectors
    Fail_Dets = inc_timeout_fd:start(Links),
    receive after 100 -> pass end,
    %% Down, leader detectors
    LDs = monarch_eld:start(Fail_Dets, Links),
    receive after 100 -> pass end,
    %% Best effort broadcasts
    Bebs = beb:start(Links),
    receive after 100 -> pass end,
    %% The epoch change
    Epoch_Changes = epoch_change:start(LDs, Bebs, Links),
    receive after 100 -> pass end,
    %% The initial states of the epoch consensus
    E_States = [{0, bottom} || _ <- Bebs],
    %% The epoch consensus
    Epoch_Conss = [_E1, _E2, _E3] = rw_epoch_cons:start(Bebs, Links, 0, E_States),
    receive after 100 -> pass end,
    %% The ldc object
    _LDCs = [A, _B, _C] = ld_cons:start(Epoch_Conss, Epoch_Changes),

    %% Debug mode
    dbg:tracer(),
    dbg:p(A,m),
    %dbg:p(E1,m),

    receive after 100 -> pass end,
    A ! {propose, myval}.

tob() ->

    Links = link:perfect_link([node(), node(), node()]),
    receive after 100 -> pass end,
    Fail_Dets = inc_timeout_fd:start(Links),
    receive after 100 -> pass end,
    LDs = monarch_eld:start(Fail_Dets, Links),
    receive after 100 -> pass end,
    Bebs = beb:start(Links),
    receive after 100 -> pass end,
    Epoch_Changes = epoch_change:start(LDs, Bebs, Links),
    receive after 100 -> pass end,
    E_States = [{0, bottom} || _ <- Bebs],
    Epoch_Conss = rw_epoch_cons:start(Bebs, Links, 0, E_States),
    receive after 100 -> pass end,
    %% The ldc object
    LDCs = ld_cons:start(Epoch_Conss, Epoch_Changes),

    %% Eager reliable broadcast
    ERBs = erb:start(Bebs),
    receive after 100 -> pass end,
    %% Total order broadcast
    _TOBs = [A, _B, _C] = tob:start(ERBs, LDCs),

    %% Debug mode
    dbg:tracer(),
    dbg:p(A,m),
    
    receive after 100 -> pass end,
    A ! {broadcast, self(), coucou_le_monde}.

bank() ->
    dbg:tracer(),
    Nodes = [node(), node(), node()],
    Links = link:perfect_link(Nodes),
    Bebs = beb:start(Links),
    RBs = [R1, _R2, _R3] = erb:start(Bebs),
    dbg:p(R1,m),
    
    receive after 100 -> pass end,
    FDs = inc_timeout_fd:start(Links),
    receive after 100 -> pass end,
    LDs = monarch_eld:start(FDs, Links),
    receive after 100 -> pass end,
    RW_Epochs_Cons = rw_epoch_cons:start(Bebs, Links, 1, [{1, bottom} || _ <- Bebs]),
    receive after 100 -> pass end,
    Epoch_Changes = epoch_change:start(LDs, Bebs, Links),
    receive after 100 -> pass end,
    Consensuss = ld_cons:start(RW_Epochs_Cons, Epoch_Changes),
    receive after 100 -> pass end,
    
    TOBs = [T1, _T2, _T3] = tob:start(RBs, Consensuss),
    dbg:p(T1,m),
    receive after 100 -> pass end,

    [A, _B, _C] = bank:start(TOBs),
    dbg:p(A,m),
    
    receive after 200 -> pass end,
    A ! {create, self(), 1, 10},

    receive after 300 -> pass end,
    A ! {create, self(), 2, 20},
    
    receive after 500 -> pass end,
    A ! {transfer, self(), 1, 2, 5}.    
    
epoch_change() ->
    dbg:tracer(),
    Links = link:perfect_link([node(), node(), node()]),
    %Links = link:damn_simple_link([node(), node(), node()]),
    Fail_Dets = inc_timeout_fd:start(Links),
    LDs  = [_L1, _L2, _L3] = monarch_eld:start(Fail_Dets, Links),
    Bebs = beb:start(Links),
    _Dev_Nulls = [utils:dev_null() || _ <- [ 1, 2, 3 ]],
    Epoch_Changes = [_C1, _C2, _C3] = epoch_change:start(LDs, Bebs, Links), 
    %[dbg:p(C,m) || C <- Epoch_Changes], 
    %[E ! {subscribe, O} || {E, O} <- lists:zip(Epoch_Changes, _Dev_Nulls)],
    %[dbg:p(O, m) || O <- _Dev_Nulls], 
    Epoch_Changes.

rwec() ->
    %% The links
    Links = link:perfect_link([node(), node(), node()]),
    receive after 100 -> pass end,
    %% Best effort broadcasts
    Bebs = beb:start(Links),
    receive after 100 -> pass end,
    %% The epoch change
    %% The initial states of the epoch consensus
    E_States = [{0, bottom} || _ <- Bebs],
    %% The epoch consensus
    Epoch_Conss = [_E1, _E2, _E3] = rw_epoch_cons:start(Bebs, Links, 0, E_States),

    %% Debug mode
    dbg:tracer(),
    dbg:p(_E1,m),

    Epoch_Conss.
