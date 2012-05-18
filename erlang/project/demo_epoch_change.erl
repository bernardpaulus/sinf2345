-module(demo_epoch_change).
-compile(export_all).

dslinks(N) ->
    Links = link:damn_simple_link([node() || _ <- lists:seq(1, N)]),
    test(Links, N).

perfect_link(N) ->
 
    Links = link:perfect_link([node() || _ <- lists:seq(1, N)]),
    test(Links, N).

test(Links, N) ->
    Bebs = beb:start(Links),
    Fail_Dets = inc_timeout_fd:start(Links),
    Mon_ELD = monarch_eld:start(Fail_Dets, Links),
    Rs = [utils:dummy_receiver() || _ <- lists:seq(1, N)],
    Echs = epoch_change:start(Mon_ELD, Bebs, Links),
    receive after 100 -> pass end,
    [utils:subscribe(R, Ech) || {Ech, R} <- lists:zip(Echs, Rs)],
    {echs, Echs, rs, Rs}.
