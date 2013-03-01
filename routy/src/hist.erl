-module(hist).
-export([new/1, update/3]).


new(Name) ->
    [{Name, L=old}].


%% you can update the set of messages and the 
%% returned value is either a new set or the 
%% atom old.
    
update(Node, N, History)->
    case lists:keysearch(Node, 1, History) of
	{value, {HistNode, HistN}} ->
	    if 
		N > HistN -> 
		    {new, [{Node, N}|lists:keydelete(Node, 1, History)]};
		true -> 
		    old
	    end;
	false ->
	    {new, [{Node, N}|History]}
    end.