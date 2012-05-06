% @doc a condition server
-module(condition).
-export([start/0, stop/0, upon/2, check/1, clear/0]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

% module interface
start() -> 
    case gen_server:start({local, condition}, ?MODULE, arguments , []) of
        {ok, Pid} -> Pid;
        {error, {already_started,Pid}} -> Pid
    end.

stop() -> gen_server:cast(condition, stop).

% stores, for this process, a condition that will be executed the first time it is true
upon(Condition, Action) when 
        is_function(Condition, 1), 
        is_function(Action, 1) ->
    gen_server:call(condition, {upon, Condition, Action}).

% check the conditions
check(State) ->
    gen_server:call(condition, {check, State}).

% clear my existing conditions
clear() ->
    gen_server:call(condition, {clear}).


% callbacks
init(_Args) -> {ok, dict:new()}.

handle_call({upon, Condition, Action}, {Pid, _Tag},  State) when
        is_function(Condition, 1), 
        is_function(Action, 1) ->
    {reply, ok, dict:append(Pid, {Condition, Action}, State)};

handle_call({check, S}, {Pid, _Tag}, State) ->
    case dict:find(Pid, State) of 
        {ok, V} ->
            {Ok, Later} = lists:partition(
                fun({Condition, _}) ->
                    apply(Condition, [S]) 
                end, V),
            lists:foreach(fun({_, Action}) -> apply(Action, [S]) end, Ok),
            case Later of
                [] -> {reply, ok, dict:erase(Pid, State)};
                [_ | _ ] -> {reply, ok, dict:store(Pid, Later, State)}
            end;
        error -> 
            {reply, {error, Pid, has_no_condition}, State}
    end;

handle_call({clear}, {Pid, _Tag}, State) ->
    {reply, ok, dict:erase(Pid, State)}.

handle_cast(stop, _State) ->
    {stop, normal, stopped}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
