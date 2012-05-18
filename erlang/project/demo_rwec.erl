-module(demo_rwec).
-compile(export_all).

start(N) ->
    %% The links
    Links = link:perfect_link([ node() || _ <- lists:seq(1, N)]),
    receive after 100 -> pass end,
    Bebs = beb:start(Links),
    Rs = [utils:dummy_receiver() || _ <- lists:seq(1, N)],
    E_States = [{0, bottom} || _ <- Bebs],
    receive after 100 -> pass end,
    Epoch_Conss = [_E1, _E2, _E3] = rw_epoch_cons:start(Bebs, Links, 0, E_States),
    [utils:subscribe(R, Ec) || {Ec, R} <- lists:zip(Epoch_Conss, Rs)],
    receive after 100 -> pass end,
    hd(Epoch_Conss) ! {propose, bonjour},
    
    {econs, Epoch_Conss, rs, Rs}.
