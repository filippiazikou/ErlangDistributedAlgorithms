- module(key).
-export([generate/0, between/3]).

generate() ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	random:uniform(100000000).

between(Key, From, To) ->
	if 
		(To<From) and (Key=<To)->
			true;
		(To<From) and (Key>From)->
			true;
		(To>From) and (Key>From) and (Key=<To) ->
		   true;
		(To==From) ->
			true;
		true ->
		   false
	end.