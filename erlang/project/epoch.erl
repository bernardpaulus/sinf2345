% @doc Leader-based Epoch Change
% based on algo 5.5 p 219
% @author bernard paulus
% @author martin trigaux

-module(epoch).
-compile(export_all).
-import(spawn_utils, [spawn_multiple_on_top/3]).
