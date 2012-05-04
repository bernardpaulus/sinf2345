-module(test).
-compile(export_all).

test_ldc() ->
    %% The links
    Links = [L1, L2, L3] = link:perfect_link([node(), node(), node()]),

    %% Down, leader detectors
    Down = _,

    %% Best effort broadcast
    Beb = _,
    
    %% The epoch change
    Ep_Change = epoch_change:start(Downs, Beb, Links).
