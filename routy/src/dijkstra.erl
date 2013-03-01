-module(dijkstra).
-export([iterate/3, table/2, route/2]).


 update(Node, N, Gateway, Sorted) ->
	case entry(Node, Sorted) of
		0 ->
			Sorted;
    	_ ->
    		replace(Node, N, Gateway, Sorted)
	end.

%%% Returns the length of the shortest path to the node or 0 if the node is not found.
entry(Node, Sorted) ->
	case lists:keysearch(Node,1,Sorted) of
		{value,Answer} ->
			{N, L, G} = Answer,
			L;
    	false ->
    		0
	end.

%%% replaces the entry for Node in Sorted with a new entry having a new length N and Gateway. 
%%% The resulting list should of course be sorted.
replace(Node, N, Gateway, Sorted) ->
	case lists:keysearch(Node,1,Sorted) of
		{value,Answer} ->
			{No, L, G} = Answer,
			if 
				N<L ->
					lists:keysort(2, [{Node, N, Gateway} | lists:keydelete(Node, 1, Sorted)]);
				true ->
					Sorted
			end;
    	false ->
    		Sorted
	end.


%%% Construct a table given a sorted list of nodes, a map and a table 
%%%	constructed so far.
iterate(Sorted, Map, Table) ->
	%%%for each node in Sorted
	case Sorted of
		[] ->
			Table;
		[H|T] ->
			case H of
				{_, inf,unknown} ->
					Table;
				{Node, Length, Gateway} ->
					R = map:reachable(Node, Map),
					case R of
						0 ->
							iterate(T, Map, Table);
						_ ->
							NewSorted = updateSorted(R, Length+1, Gateway, Sorted),
							iterate(lists:keydelete(Node, 1, NewSorted), Map, [{Node, Gateway}|Table])
					end
			end
	end.

updateSorted (R, L, Node, Sorted) ->
	case R of
		[] ->
			Sorted;
		[H|T] ->
			NewSorted = update(H, L, Node, Sorted),
			updateSorted(T,L,Node, NewSorted)
	end.

%%%Converts list of Names to a List of tuples
convert_list(R, Node) ->
	convert_list(R, Node, []).
convert_list(R, Node, Rtuple) ->
	case R of
		[] ->
			Rtuple;
		[H|T] ->
			convert_list(T, Node, [{H, Node}|Rtuple])
	end.


table(Gateways, Map) ->
	Cities = map:all_nodes(Map),
	table(Gateways, Map, init_sorted(Cities, [])).
table([], Map, Sorted) ->
	iterate(Sorted, Map, []);
table(Gateways, Map, Sorted) ->
	[H|T] = Gateways,
	%%table(T, Map, update(H, 0, H, Sorted)).
	case lists:keyfind(H, 1, Map) of
		Answer ->
			{Node, Links} = Answer,
			table(T, Map, add_links(H,Links,update(H, 0, H, Sorted)));
    	false ->
    		table(T, Map, update(H, 0, H, Sorted))
	end.
	
add_links(Node, Links, Sorted) ->
	case Links of
		[] ->
			Sorted;
		[H|T] ->
			add_links(Node, T, update(H, 1, Node, Sorted))
	end.
	
init_sorted([], Sorted) ->
	Sorted;
init_sorted(Cities, Sorted) ->
	[H|T] = Cities,
	init_sorted(T, [{H, inf, unknown} | Sorted]).

route(Node, Table) ->
	case lists:keyfind(Node, 1, Table) of
		Answer ->
			{Node, Gateway} = Answer,
			Gateway;
    	false ->
    		not_found
	end.
