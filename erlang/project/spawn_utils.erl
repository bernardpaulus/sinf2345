% @doc module contaiing all the utilitary functions relative to spawning.
% @author bernard paulus

-module(spawn_utils).
-export([
        spawn_register/2, 
        spawn_same_node/2,
        spawn_same_node/3,
        spawn_multiple/2,
        spawn_multiple/3,
        spawn_multiple_on_top/2,
        spawn_multiple_on_top/3
        ]).


% @spec (Proc_Name :: atom() , Fun :: () -> term() ) -> pid() | false
% @doc safely spawn and register a process under name ProcName
% returns the pid if successfully spawned, false otherwise
spawn_register(Proc_Name, Fun) ->
    Pid = spawn(
            fun() -> 
                receive 
                    register_ok -> Fun(); % only start Fun when registered
                    register_ko -> exit(normal)
                end
            end
        ),
    try register(Proc_Name, Pid) of
        true -> Pid ! register_ok,
                Pid % return pid
    catch 
        error:badarg -> 
            Pid ! register_ko,
            false % badarg is thrown when there exist a Pid that is
                  % already registered (and alive)
    end.


% @spec (Nodes, Funs, Argss) -> [Pids :: pid()]
%   Nodes = [node()]
%   Funs = [Fun :: function()]
%   Argss = [Args]
%   Args = [term()]
% @doc for each (Node, Fun, Args), spawn Fun on node Node with [Pids | Args] as
% argument.
% Pids is the list of the Pid of all those spawned processes.
spawn_multiple(Nodes, Funs, Argss) when 
        is_list(Nodes), is_list(Funs), is_list(Argss),
        length(Nodes) == length(Funs), length(Funs) == length(Argss) ->
    Parent = self(),
    % acknowledge, then start Fun with the Pids and the args as argument
    Start_Fun = fun(Fun, Args) ->
            fun() -> 
                receive {Parent, Pids} -> 
                    Parent ! {ack, self()},
                    apply(Fun, [Pids | Args])
                end
            end
        end,
    Pids = [spawn(Node, Start_Fun(Fun, Args)) || 
                    {Node, Fun, Args} <- lists:zip3(Nodes, Funs, Argss)],
    % send list of Pids
    [ Pid ! {Parent, Pids} || Pid <- Pids ],
    % ack
    [receive {ack, Pid} -> ok end || Pid <- Pids],
    Pids.

% @spec (Nodes, Funs) -> [Pids :: pid()]
%   Nodes = [node()]
%   Funs = [Fun :: function()]
% @equiv spawn_multiple(Nodes, Funs, [[] || _ <- lists:seq(1, length(Funs))])
spawn_multiple(Nodes, Funs) when is_list(Nodes), is_list(Funs), 
        length(Nodes) == length(Funs) ->
    spawn_multiple(Nodes, Funs, [[] || _ <- lists:seq(1, length(Funs))]).


% @spec (Downs, Funs) -> [Pids :: pid()]
%   Downs = [Down :: pid()]
%   Funs = [Fun :: function()]
% @equiv spawn_multiple_on_top(Downs, Funs, [[] || _ <- lists:seq(1, length(Funs))])
spawn_multiple_on_top(Downs, Funs) when is_list(Downs), is_list(Funs), 
        length(Downs) == length(Funs) ->
    spawn_multiple_on_top(Downs, Funs, [[] || _ <- lists:seq(1, length(Funs))]).

% @spec (Downs, Funs, Argss) -> [Pids :: pid()]
%   Downs = [Down :: pid()]
%   Funs = [Fun :: function()]
%   Argss = [Args]
%   Args = [term()]
% @doc spawn multiple processes, each on top of a process Down.
% spawn each Fun on the same node as the corresponding Down, passing 
% [Others, Down | Args] as argument.
spawn_multiple_on_top(Downs, Funs, Argss) when
        is_list(Downs), is_list(Funs), is_list(Argss),
        length(Downs) == length(Funs), length(Funs) == length(Argss) ->
    Nodes = [node(Down) || Down <- Downs],
    Argss1 = [ [Down | Args] || {Down, Args} <- lists:zip(Downs, Argss)],
    spawn_multiple(Nodes, Funs, Argss1).


% @spec (Down, Fun) -> pid()
%   Down = pid(),
%   Fun = () -> T
% @equiv spawn_same_node(Down, Fun, [])
% @doc spawns Fun on the same node as Down.
spawn_same_node(Down, Fun) when is_function(Fun, 0) ->
    spawn_same_node(Down, Fun, []).

% @spec (Down, Fun, Args) -> pid()
%   Down = pid(),
%   Fun = function(),
%   Args = [term()]
% @doc spawns Fun with the list of arguments Args on the same node as Down.
spawn_same_node(Down, Fun, Args) when is_function(Fun), is_list(Args) ->
    Parent = self(),
    Pid = spawn(node(Down), fun() ->
            Parent ! {ack, self()},
            apply(Fun, Args)
        end),
    % wait for ack
    receive {ack, Pid} -> ok end,
    Pid.

