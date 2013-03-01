-module(node2).
-export([node/4, start/1, start/2]).
-define(Stabilize, 1000).
-define(Timeout, 10000).

start(Id) ->
	start(Id,nil).

%id is the key:generate and Peer is the Pid of the connected node
start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
	{ok, {Id, self()}};
connect(Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok, {Skey, Peer}}
	after ?Timeout ->
		io:format("Time out: no response~n",[])
	end.



node(Id, Predecessor, Successor, Store) ->
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Store);
		{notify, New} ->
			{PredKey, PredId, NewStore} = notify(New, Id, Predecessor, Store),
			Pred = {PredKey, PredId},
			node(Id, Pred, Successor, NewStore);
		{request, Peer} -> 
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor, Store);
		{status, Pred} -> 
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ, Store);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor, Store);
		probe ->
			create_probe(Id,Successor),
			node(Id, Predecessor, Successor, Store);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor, Store);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor, Store);
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Added);
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Store);
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(Id, Predecessor, Successor, Merged)
	end.



add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case (Key>Pkey andalso Key=<Id) of
		true ->
			Client ! {Qref, ok},
			storage:add(Key, Value, Store);
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
	case (Key>Pkey andalso Key=<Id) of
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			{_, Spid} = Successor,
			Spid ! {lookup, Key, Qref, Client}
	end.

create_probe(Id, {Skey, Spid}) ->
	T = erlang:now(),
	Ref = Id,
	Spid ! {probe, Ref ,[Id], T}.

forward_probe(Ref, T, Nodes, Id, {Skey, Spid}) ->
	Spid ! {probe, Ref ,[Id|Nodes], T}.

remove_probe(T, Nodes) ->
	%T2 = erlang:now(),
	%T3 = T2 - T,
	io:format("MyProbe: ~w  ~n", [Nodes]).

stabilize({_, Spid}) ->
	Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil-> %inform for our existence
			Spid ! {notify, {Id, self()}},
			Successor;
		{Id, _} -> 
			Successor;
		{Skey, _} ->
			Spid ! {notify, {Id, self()}},
			Successor;
		{Xkey, Xpid} ->
			case key:between(Xkey, Id, Skey) of
				true ->
					stabilize(Pred, Id, {Xkey, Xpid});
				false ->
					Spid ! {notify, {Id, self()}},
					Successor
			end
	end.

schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.


notify({Nkey, Npid}, Id, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Keep = handover(Store, Nkey, Npid),
			{Nkey, Npid, Keep};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Keep = handover(Store, Nkey, Npid),
					{Nkey, Npid, Keep};
				false ->
					{PredK, PredI} = Predecessor,
					{PredK, PredI, Store}
			end
	end.

handover(Store, Nkey, Npid) ->
	{Keep, Leave} = storage:split(Nkey, Store),
	Npid ! {handover, Leave},
	Keep.

