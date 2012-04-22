% utility functions

-module(utils).
-export([spawn_register/2, sleep/1, time_of_the_day/0, dummy_receiver/0]).

% safely spawn and register a process under name ProcName
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

% sleep T milliseconds
sleep(T) -> receive after T -> true end.

% pretty time
time_of_the_day() ->
    {{ _, _, _},{Hour, Minute, Second}} = erlang:localtime(),
    {_, _, Us} = now(),
    lists:concat([Hour, "h", Minute, "m", Second, "s ", Us, "us"]).

% a dummy receiver that does nothing besides printing the received messages
% returns it's Pid
dummy_receiver() -> spawn(fun() -> dummy_receiver_loop() end).

dummy_receiver_loop() ->
    receive Msg -> 
        io:format("process ~p of ~p received~n~p~n", [self(), node(), Msg]),
        dummy_receiver_loop()
    end.
