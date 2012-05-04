-module(test).
-compile(export_all).

test_ldc() ->
    %% The links
    Links = [L1, L2, L3] = link:perfect_link([node(), node(), node()]),

    %% The failure detectors
    Fail_Dets = inc_timeout_fd:start(Links),

    %% Down, leader detectors
    Down = monarch_eld:start(Fail_Dets, Links),

    %% Best effort broadcasts
    Bebs = beb:start(Links),
    
    %% The epoch change
    Ep_Change = epoch_change:start(Downs, Beb, Links),
    
    %% The epoch consensus
    Ep_Cons = rw_epoch_cons:start(X).
