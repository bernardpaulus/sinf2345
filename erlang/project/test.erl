-module(test).
-compile(export_all).

ldc() ->
    %% The links
    Links = link:perfect_link([node(), node(), node()]),
    %% The failure detectors
    Fail_Dets = inc_timeout_fd:start(Links),
    %% Down, leader detectors
    Downs = monarch_eld:start(Fail_Dets, Links),
    %% Best effort broadcasts
    Bebs = beb:start(Links),

    %% The epoch change
    Epoch_Changes = epoch_change:start(Downs, Bebs, Links),
    %% The initial states of the epoch consensus
    E_States = [{0, bottom} || _ <- Bebs],
    %% The epoch consensus
    Epoch_Conss = rw_epoch_cons:start(Bebs, Links, 0, E_States),
    
    %% The ldc object
    ld_cons:start(Epoch_Conss, Epoch_Changes).
