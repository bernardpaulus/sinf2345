-module(demo).
-import(spawn_utils, [spawn_multiple_on_top/2]).
-compile(export_all).



start(Nodes) ->
    Links = link:perfect_link(Nodes),
    Bebs = beb:start(Links),
    FDs = inc_timeout_fd:start(Links),
    LDs = monarch_eld:start(FDs, Links),
    RW_Epochs_Cons = rw_epoch_cons:start(Bebs, Links, 1, [{1, bottom} || _ <- Bebs]),
    Epoch_Changes = epoch_change:start(LDs, Bebs, Links),
    ld_cons:start(RW_Epochs_Cons, Epoch_Changes).
    

propose(LDCs, Value) ->
    [H | _] = LDCs,

    H ! {subscribe, self()},
    H ! {propose, Value},
    
    receive
        {decide, Val, Round} ->
            io:format("Decided value ~p after round ~p ~n", [Val, Round])
    end.
