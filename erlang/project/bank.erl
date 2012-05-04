-module(bank).
-import(spawn_utils, [spawn_multiple_on_top/2]).

-record(bank_state, {
          tob = none,
          accounts = dict:new(),
          pendings = []}).

start(TOBs)
  when is_pid(hd(TOBs)) ->
    spawn_multiple_on_top(TOBs, [fun init/2 || _ <- TOBs]).

start() ->
	Nodes = [node(), node(), node()],
	TOBs = tob:start(Nodes),
    start(TOBs).
    
init(_Peers, TOB) ->
    Self = self(),
    condition:start(),
    condition:upon(
      fun(#bank_state{accounts = Accounts}, Account_ID, Amount) ->
              Total = dict:fetch(Account_ID, Accounts),
              (Total + Amount > 0)
      end
      fun(State) ->
              Self ! {negative_account_fee}
      end),
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
            
            loop(State#bank_state{accounts = New_Acc});
        
        {broadcast_create, Amount} ->
            #bank_state{tob = TOB, pendings = P, accounts = Accounts} = State,
            ID = next_account(Accounts),
            TOB ! {broadcast, self(), {create, ID, Amount}},
            loop(State#bank_state{accounts = New_Acc,
                                  pendings = []})
                
    end.

next_account(Accounts) -> next_account(Accounts, 0).
next_account([], Tot) -> Tot +1;
next_account([H | T], Tot) -> next_account(T, Tot+1).
                         

create_user(Amount) ->
    self() ! {broadcast_create, Amount}.

transfer_amount(From, To, Amount) ->
    self() ! {broadcast_transfer, From, To, Amount}.
    
