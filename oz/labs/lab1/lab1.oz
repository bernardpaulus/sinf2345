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
functor

import
    Application
    Utils at './Utils.ozf'
    System
    OS
    Connection

define
    Print = System.show 
    NewPortObject = Utils.newPortObject
    DelayMessage = Utils.delayMessage
    Random = Utils.random
    

    % create a new agent
    fun {NewAgent Name Neighbours Maxstate}

        % initialize state
        fun {Init}
            0
        end

        proc {SendAll Neighbours State ?NewState}
            case Neighbours of
                H | T then {DelayMessage H msg(Name State + 1) {Random 1000}}
                           {SendAll T State + 1 NewState}
            [] nil then NewState = State end 
        end

        proc {AgentProc Msg State ?NewState}
            if State >= Maxstate then
                {Thread.terminate {Thread.this $}}
            end
            case Msg of
                 msg(?_ ?Clock) then
                    {Print [Name received(Msg)]}
                    if Clock > State then
                        {SendAll Neighbours Clock NewState}
                    else
                        {SendAll Neighbours State NewState}
                    end
            end
        end
    in
        local
            InitState = {Init }
            Port = {NewPortObject AgentProc InitState}
        in    
            proc {$ Msg}
                case Msg of
                    name(?Ret) then Ret = Name
                []  msg(?From ?Clock) 
                    then {Send Port msg(From Clock)} end
            end
        end
    end
            
    A B C %Ret
    Aticket Bticket Cticket
in
    {OS.srand 0}
    % Connection.offer must be done before Connection.take, otherwise the
    % thread waits for Bticket to be bound
    Aticket = {Connection.offerMany A}
    Bticket = {Connection.offerMany B}
    Cticket = {Connection.offerMany C}
    A = {NewAgent a [{Connection.take Bticket} {Connection.take Cticket}] 10}
    B = {NewAgent b [{Connection.take Aticket} {Connection.take Cticket}] 10}
    C = {NewAgent c [{Connection.take Aticket} {Connection.take Bticket}] 10}
    %{A name(Ret)}
    %{Print Ret}
    {B msg(z 0)}
    {Delay 5000} 
        % could've add an additional variable bound on thread exit, but i'm lazy

    {Application.exit 0}
end
