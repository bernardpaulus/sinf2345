% Miscellaneous utilities
% @authors Bernard Paulus and Xavier Dubruille
% @version 1.0
functor

import
   OS
	System
	Browser
export
   Random
   MakeMultiTuple % ?
   MakeGrid
   NewPortObject
   DelayMessage 
   ForAllAcc
   ForAcc
   NewPointOr
   Exclude 
   Print
   Browse
	RandomOnList
define

   % Fetch a random natural number
   % @param N   Upper bound of the random number
   % @return A random integer number between 0 and N-1 included
   fun {Random N}
      {OS.rand} mod N
   end


	% randomly choose a value in tuple with a probability
%	proc {ChooseWithProb Tuple ?Ret}
%
%		for I in 1..{Width Tuple}
%			case Tuble.I of Stuff#Prob then
%				if {Random 100} < Prob then Ret=Stuff end			
%			end
%		end 
%
%		{ChooseWithProb Tuple ?Ret}
%	end



   % Make a multi-dimensional tuple
   % Leaf items are left unbound
   % @param LabelDims   List of (Label#Dimension)
   %    Each dimension will have the label Label and Dimension children
   % @return Multi-dimensional tuple described by LabelDims
   proc {MakeMultiTuple LabelDims ?Result}
      case LabelDims
      of (Label#Dimension)|LabelDimsTail then
         Result = {MakeTuple Label Dimension}
         for Index in {Arity Result} do
            {MakeMultiTuple LabelDimsTail Result.Index}
         end
      [] nil then
         skip
      end
   end
   
   % Make a grid of tuples (2D-tuple)
   % Leaf items are left unbound
   % @param Size = size(width:Width height:Height)
   %    Size of the grid
   % @return Grid whit size Size
   fun {MakeGrid Size}
      {MakeMultiTuple [grid#(Size.width) column#(Size.height)]}
   end

   % NewPortObject
   local
      % Message handling loop
      % @param MessageFun    Message handling function
      % @param State         Current state
      % @param InputStream   Input stream
      proc {MsgLoop MessageFun State InputStream}
         case InputStream
         of Message|InputStreamTail then
            NewState = {MessageFun Message State}
         in
            {MsgLoop MessageFun NewState InputStreamTail}
         end
      end
   in
      % Create a new port object
      % A port object is a port whose messages are handled by a
      % dedicated function.
      % @param MessageFun = <P/3>(Message CurState ?NewState)
      %    Message handling function, called for each message
      % @param InitState   Initial state of the message function
      % @return Created port
      proc {NewPortObject MessageFun InitState ?Port}
         thread
            InputStream
         in
            Port = {NewPort InputStream}
            {MsgLoop MessageFun InitState InputStream}
         end
      end
   end


	fun {RandomOnList ListIn}
		T = {List.toTuple '#' ListIn}
		W = {Width T} 
		R = {Random W} + 1
		in
		T.R

	end

   % Send a message to an object with after a non-blocking delay
   % @param Object    Object which send the message to
   % @param Message   Message to be sent
   % @param Time      Time (in miliseconds) to delay
   proc {DelayMessage Object Message Time}
      thread
         {Delay Time}
         {Object Message}
      end
   end

	% NewPointOr
	% 
	% A point manipulation abstraction, allowing differential operations
	% 	relatively to the point and it's orientation
	fun {NewPointOr X Y Or}
		Right=0
		Up=1
		Left=2
		Down=3

		fun {NewX X}
			{NewPointOr X Y Or}
		end

		fun {NewY Y}
			{NewPointOr X Y Or}
		end

		fun {NewOr Or}
			{NewPointOr X Y Or}
		end

		% Moves forward according to the current orientation of the point
		% @param Delta : the added number in the direction of the point
		% @return: a new point at the destination of the move
		fun {Forward Delta}
			if Or == Right
			then {NewX X+Delta}
			elseif Or == Up
			then {NewY Y+Delta}
			elseif Or == Left
			then {NewX X-Delta}
			elseif Or == Down
			then {NewY Y-Delta}
			end
		end

		% Move to the right according to the current orientation of the point
		% i.e. moves to the right when directioin is Up, down when Right
		% @param delta : the added int in the direction 
		%	perpendicular to the right of the current orientation
		% @return : a new point at the new coordinates
		fun {ToRight Delta}
			if Or == Right
			then {NewY Y-Delta}
			elseif Or == Up
			then {NewX X+Delta}
			elseif Or == Left
			then {NewY Y+Delta}
			elseif Or == Down
			then {NewX X-Delta}
			end
		end

		% this adds the deltas X and Y contained in point P
		% Without taking P's direction into account
		%	i.e. this function is the combinasion of the two above
		% @param P: the point whose coordinates are the delta to add 
		% 	to the current coordinates.
		% @return : a new point at the new coordinates
		fun {AddDelta P}
			{{{NewPointOr X Y Or}.toRight P.x}.forward P.y}
		end

		fun {Opposite}
			{NewPointOr ~X ~Y (Or + 2) mod 4}
		end

		% Sums the coordinates but keeps current orientation
		fun {SumXY P}
			{NewPointOr P.x + X P.y + Y Or}
		end
	in
		point(x:X y:Y o:Or mod 4 
			right:Right up:Up left:Left down:Down
			newX:NewX newY:NewY newOr:NewOr 
			forward:Forward toRight:ToRight
			addDelta:AddDelta opposite:Opposite
			sumXY:SumXY)
	end

	proc {ForAllAcc L P In ?Out}
		case L of
			nil then Out=In []
			X|T then Mid in
				{P In X Mid}
				{ForAllAcc T P Mid Out}
		end
	end

	proc {ForAcc A B S P In ?Out}
		proc {LoopUp C In ?Out}
			Mid in
			if C =< B then
				{P In C Mid} {LoopUp C+S Mid Out}
			else In=Out end
		end
		proc {LoopDown C In ?Out}
			Mid in
			if C =< B then
				{P In C Mid} {LoopUp C+S Mid Out}
			else In=Out end
		end
	in
		if S > 0 then {LoopUp A In Out} end
		if S < 0 then {LoopDown A In Out} end
	end


	% Exclude each Point of the exclusion list Xl1 in the list L2 
	% 	Comparing only x and y coordinates
	fun {Exclude Xl1 L2 }
		case L2 of
			H | T then if {Some Xl1 fun {$ E} 
						H.x == E.x andthen H.y == E.y end}
					then {Exclude Xl1 T }
					else H | {Exclude Xl1 T } end []
			nil then nil
		end
	end
	
	% ALias fors the system.shoInfo Call
	proc {Print X}
		{System.showInfo X}
	end

	proc {Browse X}
		{Browser.browse X}
	end
in
	{OS.srand 0}
end
