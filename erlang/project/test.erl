-module(test).
-compile(export_all).

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
    Epoch_Conss = [E1, _E2, _E3] = rw_epoch_cons:start(Bebs, Links, 0, E_States),
    receive after 100 -> pass end,
    %% The ldc object
    _LDCs = [A, _B, _C] = ld_cons:start(Epoch_Conss, Epoch_Changes),

    %% Debug mode
    dbg:tracer(),
    dbg:p(A,m),
    dbg:p(E1,m),

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
    Nodes = [node(), node(), node()],
    Links = link:perfect_link(Nodes),
    Bebs = beb:start(Links),
    RBs = [R1, _R2, _R3] = erb:start(Bebs),
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
    [A, _B, _C] = bank:start(TOBs),

    dbg:tracer(),
    dbg:p(T1,m),
    dbg:p(A,m),
    dbg:p(R1,m),
    
    A ! {create, self(), 1, 10}.
    
