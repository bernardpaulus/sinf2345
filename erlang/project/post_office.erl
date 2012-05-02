-module(post_office).

-export([post_office/2]).

post_office(PO_List, Link) ->
    Start_loop = fun() ->
        % create empty post office
        post_office_loop(sets:new(), dict:new(), PO_List, Link) 
    end,
    spawn(Start_loop).

post_office_loop(My_Users, All_Users, PO_List, My_Link) ->
    receive
        % add an internal new user
        {subscribe, User_Pid} ->
            % check avoid double subscription
            case sets:is_element(User_Pid, My_Users) of
                true ->
                    io:format("The user ~p has already subscribed to me~n", [User_Pid]),
                    post_office_loop(My_Users, All_Users, PO_List, My_Link);
                false ->
                    list:foreach(
                        % to all post office, send a subscribe external user
                        fun(Post_Office) ->
                            % encapsulated message
                            My_Link ! {transmit, self(), Post_Office, {subscribe, User_Pid, self()}}
                        end,
                        PO_List),
                    post_office_loop(
                        sets:add_element(User_Pid,My_Users),
                        dict:append(User_Pid, self(), All_Users),
                        PO_List,
                        My_Link)
            end;


        % a user send a message to another user
        {send, From_User, To_User, M} ->
            Key = dict:find(To_User, All_Users),
            if
                Key == ok ->
                    Dest_PO = dict:fetch(To_User, All_Users),
                    My_Link ! {send, self(), Dest_PO, {send, From_User, To_User, M}};
                true ->
                    io:format("Oups I don't know ~p~n", [To_User])
            end,
            post_office_loop(My_Users, All_Users, PO_List, My_Link);
            
        % a link deliver a message to the post office
        {deliver, _From_PO, _To_PO, M} ->
            {Action, Body} = M,
            if
                % transmit the message to the corresponding user
                Action == send ->
                    {From_User, To_User, Enc_Msg} = Body,
                    % check the user belongs to me
                    case sets:is_element(To_User, My_Users) of
                        true ->
                            To_User ! {deliver, From_User, To_User, Enc_Msg};
                        false ->
                            io:format("The user ~p does not belong to me~n", [To_User])
                    end,
                    post_office_loop(My_Users, All_Users, PO_List, My_Link);

                % add an external user to the all users list
                Action == subscribe ->
                    {User_Pid, PO_Pid, _} = Body,
                    post_office_loop(
                        My_Users,
                        % add the user to the dic All_Users
                        dict:append(User_Pid, PO_Pid, All_Users),
                        PO_List,
                        My_Link);

                true ->
                    io:format("Unknown action ~p~n", [Action]),
                    post_office_loop(My_Users, All_Users, PO_List, My_Link)
            end
        
    end.


