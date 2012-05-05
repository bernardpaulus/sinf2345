-module(bank).
-import(spawn_utils, [spawn_multiple_on_top/2]).
-compile(export_all).

-record(bank_state, {
          tob = none,
          accounts = dict:new(),
          to_ack = dict:new()}). % renamed pending

start(TOBs)
  when is_pid(hd(TOBs)) ->
    spawn_multiple_on_top(TOBs, [fun init/2 || _ <- TOBs]).

start() ->
	Nodes = [node(), node(), node()],
	TOBs = tob:start(Nodes),
    start(TOBs).
    
init(_Peers, TOB) ->
    utils:subscribe(TOB),
    %Self = self(),
    %condition:start(),
    %condition:upon(
    %  fun(#bank_state{accounts = Accounts}, Account_ID, Amount) ->
    %          Total = dict:fetch(Account_ID, Accounts),
    %          (Total + Amount > 0)
    %  end
    %  fun(State) ->
    %          Self ! {negative_account_fee}
    %  end),
	loop(#bank_state{tob = TOB}).


loop(State) ->
	receive
        %% from the user: he wants to create a new account
        {create, From_Pid, _Account_ID, _Amount} = M ->
            #bank_state{tob = TOB} = State,
            TOB ! {broadcast, self(), M},
            To_Ack = dict:append(From_Pid, M, State#bank_state.to_ack),
            loop(State#bank_state{to_ack = To_Ack});
            %#bank_state{accounts = Accounts} = State,
            %New_Acc = dict:append(Account_ID, Amount, Accounts),
            %loop(State#bank_state{accounts = New_Acc});

        % from the TOB: receive a deliver create account
        %  => create the account for real
        {deliver, _From, {create, From_Pid, Account_ID, Amount} = M} ->
            % we are guaranteed that the execution of this part will be executed
            % the same way on every bank process since the TOB processes agreed
            % on an order to deliver their messages. (erlang sends are FIFO
            % between any pair of correct processes)
            #bank_state{accounts = Accounts, to_ack = To_Ack} = State,
            case dict:find(Account_ID, Accounts) of
                {ok, _Money} ->
                    % account already exist in the db => do not perform request
                    Accounts1 = Accounts,
                    Reply = {error, account_already_exists, M};
                error ->
                    % no other account with Account_ID => create
                    Accounts1 = dict:store(Account_ID, Amount, Accounts),
                    Reply = {ok, account_created, M}
            end,
            % check whether to reply (if the user sent {create...} to self() )
            case dict:find(From_Pid, To_Ack) of
                {ok, Msg_List} ->
                    Need_Reply = lists:any(fun (X) -> X == M end, Msg_List);
                error ->
                    Msg_List = [], % suppress compiler warning "unsafe Msg_List"
                    Need_Reply = false
            end,
            % send reply/ack if needed
            case Need_Reply of
                true ->
                    % remove the messages from the list of messages to ack for a
                    % certain Pid
                    From_Pid ! Reply,
                    To_Ack1 = dict:store(From_Pid, lists:delete(M, Msg_List),
                                    To_Ack),
                    loop(State#bank_state{
                            accounts = Accounts1,
                            to_ack = To_Ack1});
                false ->
                    loop(State#bank_state{
                            accounts = Accounts1})
            end;
            
        
        %% request a change in the amount of an account
        {add, From_Pid, _Account, _Amount} = M ->
            #bank_state{tob = TOB} = State,
            TOB ! {broadcast, self(), M},
            To_Ack = dict:append(From_Pid, M, State#bank_state.to_ack),
            loop(State#bank_state{to_ack = To_Ack});
            %% no computation here: transaction not accepted yet in the TOB!
            %#bank_state{accounts = Accounts} = State,
            %Total = dict:fetch(Account, Accounts),
            %New_Acc = dict:update(Account,
            %                      fun(Amount) -> Total + Amount end,
            %                      Accounts),
            %
            %loop(State#bank_state{accounts = New_Acc});

        %% from the TOB: receive a deliver add amount
        %%  => update the account for real
        {deliver, _From, {add, From_Pid, Account_ID, Amount} = M} ->
            %% we are guaranteed that the execution of this part will be executed
            %% the same way on every bank process since the TOB processes agreed
            %% on an order to deliver their messages. (erlang sends are FIFO
            %% between any pair of correct processes)
            #bank_state{accounts = Accounts, to_ack = To_Ack} = State,
            New_Accounts = case dict:is_key(Account_ID, Accounts) of
                               true ->
                                   Reply = {ok, account_updated, M},
                                   %% update the money on the account
                                   dict:update(Account_ID,
                                               fun(Money) -> Money + Amount end,
                                               Accounts);

                               false ->
                                   Reply = {error, unknown_account, M},
                                   %% no account modified
                                   Accounts
                           end,
                                                       
            %% check whether to reply (if the user sent {create...} to self() )
            Need_Reply = case dict:find(From_Pid, To_Ack) of
                             {ok, Msg_List} ->
                                 lists:any(fun (X) -> X == M end, Msg_List);
                             error ->
                                 Msg_List = [], % suppress compiler warning "unsafe Msg_List"
                                 false
                         end,

            %% send reply/ack if needed
            case Need_Reply of
                true ->
                    % remove the messages from the list of messages to ack for a
                    % certain Pid
                    From_Pid ! Reply,
                    To_Ack1 = dict:store(From_Pid, lists:delete(M, Msg_List),
                                    To_Ack),
                    loop(State#bank_state{
                            accounts = New_Accounts,
                            to_ack = To_Ack1});
                false ->
                    loop(State#bank_state{
                            accounts = New_Accounts})
            end
        
        
        %{broadcast_create, Amount} ->
        %    #bank_state{tob = TOB, pendings = P, accounts = Accounts} = State,
        %    ID = next_account(Accounts),
        %    TOB ! {broadcast, self(), {create, ID, Amount}},
        %    loop(State#bank_state{accounts = New_Acc,
        %                          pendings = []})
                
    end.

%% Returns the next account number
next_account(Accounts) -> next_account(Accounts, 0).
next_account([], Tot) -> Tot +1;
next_account([_H | T], Tot) -> next_account(T, Tot+1).
                         

create_user(Amount) ->
    self() ! {broadcast_create, Amount}.

transfer_amount(From, To, Amount) ->
    self() ! {broadcast_transfer, From, To, Amount}.
    
