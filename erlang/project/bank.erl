-module(bank).
-import(spawn_utils, [spawn_multiple_on_top/2]).
-record(bank_state, {
          tob = none,
          accounts = dict:new()}).

start(TOBs)
  when is_pid(hd(TOBs)) ->
    spawn_multiple_on_top(TOBs, [fun init/2 || _ <- TOBs]).

start() ->
	Nodes = [node(), node(), node()],
	TOBs = tob:start(Nodes),
    start(TOBs).

    
init(_Peers, TOB) ->
	loop(#bank_state{tob = TOB}).


loop(State) ->
	receive
        %% create a new account
        {create, Account_ID, Amount} ->
            #bank_state{accounts = Accounts} = State,
            New_Acc = dict:append(Account_ID, Amount, Accounts),
            loop(State#bank_state{accounts = New_Acc});
        
        %% update the amount of an account to a certain value
        {transfer, Account, Amount} ->
            #bank_state{accounts = Accounts} = State,
            Total = dict:fetch(Account, Accounts),
            New_Acc = dict:update(Account,
                                  fun(Amount) -> Total + Amount end,
                                  Accounts),
            
            loop(State#bank_state{accounts = New_Acc})
        
    end.

	
    
