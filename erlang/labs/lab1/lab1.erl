% file for the first lab
%
% Assignment: create a Lamport Clock
% 
% details of the algorithm:
%   Three port objects interact with each other.
%
%   The basic algorithm is as follows:
%   Once a port object receives a message, it sends a message to each of the two
%   other port objects
%
%   Once that is done: add a sequence number/clock:
%   it is sent along with each message and it is incremented each time a process
%   sends one.
%   When a process receives a message with a clock higher than his, it updates
%   his clock to the received sequence number

% Gonna try first a single-node version
-module(lab1).
-compile(export_all).


start() ->
    N = 3, % number of agents
    Agents = [spawn(fun() -> agent(X) end) || X <- lists:seq(1,N) ],
    agent_full_mesh(Agents),
    list_send(setup_end, Agents),
    [A1|_] = Agents,
    A1 ! {0, []}.

% sleep T milliseconds
sleep(T) -> receive after T -> true end.

% send the message generated by Msg_gen to each of the Pids of the list
% Msg_gen must return {Msg, New_acc}
send_all([], _Msg_gen, _Acc) -> true;
send_all([Pid|T], Msg_gen, Acc) ->
    {Msg, New_acc} = Msg_gen(Acc),
    Pid ! Msg,
    send_all(T, Msg_gen, New_acc).


% send a message to process list
list_send(_Msg, []) -> true;
list_send(Msg, [Pid|T]) -> Pid ! Msg, list_send(Msg, T).

% for each process, send the corresponding message
bulk_send(Pid_list, Msg_list) ->
    if 
        length(Msg_list) == length(Pid_list) -> 
            bulk_send_core(Pid_list, Msg_list);
        true -> 
            erlang:error("bulk_send/2 : "
                         "list arguments must have the same length")
    end.
% bulk_send
bulk_send_core([], []) -> true;
bulk_send_core([Pid|T_pid], [Msg|T_msg]) ->
    Pid ! Msg,
    bulk_send_core(T_pid, T_msg).

% send list of messages to process
send_list_to(_Pid,[]) -> true;
send_list_to(Pid,[Msg|T]) -> Pid ! Msg, send_list_to(Pid, T).

% Connect all agents in full mesh by sending their Pids
agent_full_mesh([]) -> true;
agent_full_mesh([Pid|T]) ->
    send_list_to(Pid, T), 
        % send all remaining Pids of agents to the current agent
    list_send(Pid, T),
        % send the Pid of the current agent to the remaining ones
    agent_full_mesh(T).

% start agent 
agent(Name) -> agent_waiting_neightbours(Name, []).

% agent waits for neightbours. Neightbours should be a list of Pids.
agent_waiting_neightbours(Name, Neightbours) ->
    receive
        Pid when is_pid(Pid) -> 
            agent_waiting_neightbours(Name, [Pid | Neightbours]);
        setup_end -> 
            io:format("Agent ~p has ~p neightbours~n", 
                      [Name, length(Neightbours)]),
            agent_loop(Name, 0, Neightbours);
        Any -> 
            io:format("Agent ~p stops on unknown message '~p' received while "
                      "waiting for neightbours~n" , [Name, Any])
    end.

% main agent loop: lamport clock
agent_loop(Name, State, Neightbours) ->
    receive
        msg -> 
            io:format("Agent ~p received an old message format 'msg'~n",
                        [Name]);
        {Seq, History} when is_list(History) ->
            io:format("Agent ~p received ~p when State = ~p~n",
                       [Name, {Seq, History}, State]),
            if length(History) >= 4 -> % limit the number of retransmissions 
                    Base = nil,
                    true;
                Seq > State ->
                    Base = Seq;
                true ->
                    Base = State
            end,
            N = length(Neightbours),
            bulk_send(Neightbours, 
                      [ {Base + X, [{Name, State} | History]} || 
                        X <- lists:seq(1,N) ]),
            io:format("Agent ~p: clock ~p -> ~p~n", 
                        [Name, State, Base + N + 1]),
            agent_loop(Name, Seq + N + 1, Neightbours);
        Any -> 
            io:format("Agent ~p stops on unknown message '~p'~n"
                        , [Name, Any])
    end.

