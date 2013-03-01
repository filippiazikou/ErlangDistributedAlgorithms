- module(storage).
-export([create/0, add/3, lookup/2, split/2, merge/2]).

create() ->
	[].
add(Key, Value, List) ->
	[{Key, Value} | List].

lookup(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		{Key, Value} ->
			Value;
		false ->
			not_found
	end.

split(Key, List) ->
	{List1, List2} = lists:splitwith(fun({LKey,_}) -> LKey<Key end, List).

merge(List1, List2) ->
	lists:keymerge(1, List1, List2).