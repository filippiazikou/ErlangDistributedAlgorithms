-module(node1).
-export([node/3, start/1, start/2]).
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
	io:format("init ~w~n", [Successor]),
	node(Id, Predecessor, Successor).

connect(Id, nil) ->
	io:format("connect ~w~n", [Id]),
	{ok, {Id, self()}};
connect(Id, Peer) ->
	Qref = make_ref(),
	io:format("Format ~w~n", [Qref]),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			io:format("received key ~n"),
			{ok, {Skey, Peer}}
	after ?Timeout ->
		io:format("Time out: no response~n",[])
	end.



node(Id, Predecessor, Successor) ->
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor);
		{notify, New} ->
			Pred = notify(New, Id, Predecessor),
			node(Id, Pred, Successor);
		{request, Peer} -> 
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor);
		{status, Pred} -> 
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor);
		probe ->
			create_probe(Id,Successor),
			node(Id, Predecessor, Successor);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor)
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


notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		nil ->
			{Nkey, Npid};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					{Nkey, Npid};
				false ->
					Predecessor
			end
	end.
