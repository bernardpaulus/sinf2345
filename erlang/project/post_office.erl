-module(post_office).

-export([post_office/2]).

post_office(PO_List, Link) ->
    Start_loop = fun() ->
        post_office_loop([], PO_List, [], Link, 0) 
    end,
    spawn(Start_loop).

post_office_loop(My_Users, PO_List, All_Users, My_Link, PO_Seq) ->
    receive
        % add an internal new user
        {subscribe, User_Pid, Msg_Seq} ->
            list:foreach(
                % to all post office, send a subscribe external user
                fun(Post_Office) ->
                    % encapsulated message, the Msg_Seq is NOT incremented for 
                    % each post_office, just globally
                    Enc_Msg = {subscribe, User_Pid, self(), Msg_Seq},
                    My_Link ! {transmit, self(), Post_Office, PO_Seq, Enc_Msg}
                end,
                PO_List),
            post_office_loop(
                [User_Pid | My_Users],
                PO_List,
                dict:append(User_Pid, self(), All_Users),
                My_Link,
                PO_Seq+1);


        % a user send a message to another user
        {send, From_User, To_User, Msg_Seq, M} ->
            Key = dict:find(To_User, All_Users),
            if
                Key == ok ->
                    Dest_PO = dict:fetch(To_User, All_Users),
                    Enc_Msg = {send, From_User, To_User, Msg_Seq, M},
                    My_Link ! {transmit, self(), Dest_PO, PO_Seq, Enc_Msg},
                    post_office_loop(My_Users, PO_List, All_Users, My_Link, PO_Seq+1);
                true ->
                    io:format("Oups I don't know ~p~n", [To_User]),
                    post_office_loop(My_Users, PO_List, All_Users, My_Link, PO_Seq)
            end;
            
        % a link deliver a message to the post office
        {deliver, _From_PO, _To_PO, _Msg_Seq, M} ->
            {Action, Body} = M,
            if
                % transmit the message to the corresponding user
                Action == send ->
                    {From_User, To_User, Enc_Seq, Enc_Msg} = Body,
                    % TODO check To_User belongs to me
                    To_User ! {deliver, From_User, To_User, Enc_Seq, Enc_Msg},
                    post_office_loop(My_Users, PO_List, All_Users, My_Link, PO_Seq);
                % add an external user to the all users list
                Action == subscribe ->
                    {User_Pid, PO_Pid, _} = Body,
                    post_office_loop(
                        My_Users,
                        PO_List,
                        % add the user to the dic All_Users
                        dict:append(User_Pid, PO_Pid, All_Users),
                        My_Link,
                        PO_Seq);
                true ->
                    io:format("Unknown action ~p~n", [Action]),
                    post_office_loop(My_Users, PO_List, All_Users, My_Link, PO_Seq)
            end
        
    end.


