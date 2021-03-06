% utility functions

-module(utils).
-export([
        sleep/1, 
        dev_null/0,
        time_of_the_day/0, 
        dummy_receiver/0,
        dummy_receiver_loop/0,
        echo_proc/0,
        echo_loop/0,
        recv/0,
        recv/1,
        register_unique/2,
        seed_once/0,
        subroutine/1, subroutine/2, subroutine/3,
        subscribe/1, subscribe/2
        ]).


% sleep T milliseconds
sleep(T) -> receive after T -> true end.


% pretty time
time_of_the_day() ->
    {{ _, _, _},{Hour, Minute, Second}} = erlang:localtime(),
    {_, _, Us} = now(),
    lists:concat([Hour, "h", Minute, "m", Second, "s ", Us, "µs"]).


% a dummy receiver that does nothing besides printing the received messages
% @returns it's pid
dummy_receiver() -> spawn(fun() -> dummy_receiver_loop() end).

dummy_receiver_loop() ->
    receive Msg -> 
        io:format("process ~p of ~p received~n~p~n", [self(), node(), Msg]),
        dummy_receiver_loop()
    end.

dev_null() -> spawn(fun dev_null_loop/0).
dev_null_loop() -> receive _ -> ignore end, dev_null_loop().

% an echo process:
% upon reception of {Pid, Msg}, sends Msg to the target
% @return it's Pid
echo_proc() -> spawn(fun() -> echo_loop() end).

% loop while receiving {Pid, Msg},
% and sending Msg to Pid
echo_loop() ->
    receive {Pid, Msg} when is_pid(Pid) ->
        Pid ! Msg,
        echo_loop()
    end.


% receive any message
% @return the first message, or the atom undefined if there isn't any
recv() -> recv(0).

% receive any message before Time elapsed
% Tip: the atom infinity as argument waits infinitely long for a message
% @return the first message received before Time, 
%   or the atom undefined if there wasn't any.
recv(Time) -> receive Msg -> Msg after Time -> undefined end.


% register a unique name for this process, if it isn't registered yet
%
% try first to get the Basename
%
% @returns the name of Pid (the old name if it is already
% registered, or a new unique one otherwise).
register_unique(Basename, Pid) -> 
    try register(Basename, Pid), Basename
    catch error:badarg -> 
        % check whether process has already been registered
        case lists:filter(fun(N) -> whereis(N) == Pid end, registered()) of
            [] -> register_unique(Basename, Pid, 0);
            [Proc_Name] -> Proc_Name
        end
    end.

% register a unique name
% @return the name
register_unique(Basename, Pid, Start) ->
    Name = list_to_atom(lists:concat([Basename, "_", Start])),
    try register(Name, Pid), Name
    catch error:badarg -> register_unique(Basename, Pid, Start + 1)
    end.

% seed only one time, with now()
seed_once() ->
    case get(seeded) of
        true -> true;
        _  -> 
            {A, B, C} = now(),
            random:seed(A, B, C),
            put(seeded, true),
            ok
    end.

% spawn a function in a subprocess
% return the return value of the call
subroutine(Fun, Args) ->
    Parent = self(),
    Pid = spawn(fun() -> Ret = apply(Fun, Args), Parent ! {self(), Ret} end),
    receive
        {Pid, Ret} -> Ret
    end.

subroutine(Module, Fun, Args) ->
    Parent = self(),
    Pid = spawn(fun() -> Ret = apply(Module, Fun, Args), Parent ! {self(), Ret} end),
    receive
        {Pid, Ret} -> Ret
    end.

subroutine(Fun) ->
    Parent = self(),
    Pid = spawn(fun() -> Ret = apply(Fun, []), Parent ! {self(), Ret} end),
    receive
        {Pid, Ret} -> Ret
    end.

% @equiv subscribe(self(), Target)
subscribe(Target) -> subscribe(self(), Target).


% @spec (Pid :: pid(), Target :: pid()) -> ok
% @doc subscribes Pid at Target.
% can be stuck if Target doesn't ack
subscribe(Pid, Target) ->
    M = Target ! {subscribe, self(), Pid},
    receive {ack, Target, M} -> ok end.
