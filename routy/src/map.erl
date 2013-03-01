-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
	[].
update(Node, Links, Map) ->
	[{Node, Links} | lists:keydelete(Node, 1, Map)].

reachable(Node, Map) ->
	case lists:keysearch(Node,1,Map) of
		{value,Answer} ->
			{N, L} = Answer,
			L;
    	false ->
    		[]
	end.
	
all_nodes(Map) ->
	all_nodes(Map, []).
all_nodes([H | T], MapNames) ->
	{City, ConnectedCities} = H,	
	all_nodes(T, [City] ++ ConnectedCities ++ MapNames);
all_nodes([], MapNames) ->
	UniqMap = remove_duplicates(MapNames),
	UniqMap.

remove_duplicates(List) ->
	remove_duplicates(List, []).
remove_duplicates([H|T], UniqList) ->
	case lists:member(H, UniqList) of 
		true ->
			remove_duplicates(T, UniqList);
    	false ->
    		remove_duplicates(T, [H] ++ UniqList)		
	end;
remove_duplicates([], UniqList) ->
	UniqList.
