-module(bank).

-record(bank_state, {}).

start() ->
	Nodes = [node(), node(), node()],
	TOBs = tob:start(Nodes),
    
	loop(#bank_state{}).

loop(State) ->
	receive
        %% transfer from an account to another
        {transfer, From, To, Amount} ->
            pass;
        
        %% check the amount left on the account
        {check, From} ->
            pass;
        
        %% recieve confirmation of a transaction
        {ack, From, M} ->
            pass
    end.

	
    
