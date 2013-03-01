-module(bench).

-export([start/1, start_remote/3, start_storage/2]).

start_remote(NStart, NFinish, _) when NStart == NFinish->
	ok;
start_remote(NStart, NFinish, RemoteNode) ->
	NewNode = node2:start(NFinish, RemoteNode),
	register(list_to_atom("R" ++ integer_to_list(NFinish)), NewNode),
	start_remote(NStart, NFinish-1, RemoteNode).

start(N) ->
	BaseNode = node2:start(1),
	node_starter(N-1, BaseNode),
	BaseNode.

node_starter(0, _) ->
	ok;
node_starter(N, BaseNode) ->
	NewNode = node2:start(N, BaseNode),
	register(list_to_atom("N" ++ integer_to_list(N)), NewNode),
	node_starter(N-1, BaseNode).

start_storage(N, Node) ->
	spawn(fun() -> storage(N, Node, []) end).

storage(N, Node, Storage) ->
	io:format("~n===========================~nStoring values in the ring:~n===========================~n"),
	StoredValues = store_values(N, Node, Storage),
	io:format("~n================================~nRetrieving values from the ring:~n================================~n"),
	retrieve_values(StoredValues, Node).

store_values(0, _Node, Storage) ->
	Storage;
store_values(N, Node, Storage) ->
	Qref = make_ref(),
	Key = random:uniform(25),
	Value = random:uniform(1000),
	Data = {Key, Value},
	io:format("Storing ~w...", [Data]),
	Node ! {add, Key, Value, Qref, self()},
	receive
		{Qref, ok} ->
			io:format("[OK]~n");
		_ ->
			io:format("[FAIL]~n")
	end,
	UpdatedStorage = [Data] ++ Storage,
	store_values(N-1, Node, UpdatedStorage).

retrieve_values([], _) ->
	ok;
retrieve_values(StoredValues, Node) ->
	Qref = make_ref(),
	[Datum | Rest] = StoredValues,
	io:format("Retrieving ~w...", [Datum]),
	{DatumKey, DatumValue} = Datum,
	Node ! {lookup, DatumKey, Qref, self()},
	receive
		{Qref, Value} ->
			if
				Value == DatumValue ->
					io:format("(~w)...[OK]~n", [Value]);
				true ->
					io:format("(~w)...[FAIL]~n", [Value])
			end;
		_ ->
			io:format("[FAIL]~n")
	end,
	retrieve_values(Rest, Node).