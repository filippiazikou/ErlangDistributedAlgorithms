% (c)2011, Thomas Galliker
% Please report updates/corrections to
% thomas_galliker@bluewin.ch
% 
% Thank you

-module(tmp).
-export([start/1, start/2]).
-define(arghh, 10000).

start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    spawn_link(fun()-> init(Id, Rnd, Self) end).

init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 0, []).

start(Id, Grp) ->
    Rnd = random:uniform(1000),
    Self = self(),
    spawn_link(fun()-> init(Id, Grp, Rnd, Self) end).

init(Id, Grp, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    Self = self(),
    Grp ! {join, Self},
    receive
	{view, N, State, Leader, Peers} ->
	    Master ! {ok, State},
	    erlang:monitor(process, Leader),
	    slave(Id, Master, Leader, N, {empty}, Peers)
    after 5000 ->
	    Master ! {error, "no reply from leader"}	      
    end.

slave(Id, Master, Leader, N, Last, Peers) ->
    receive
	{mcast, Msg} ->
	    Leader ! {mcast, Msg},
	    slave(Id, Master, Leader, N, Last, Peers);
	{join, Peer} ->
	    io:format("~p (Slave): Join peer ~p~n", [Id, Peer]),
	    Leader ! {join, Peer},
	    slave(Id, Master, Leader, N, Last, Peers);
	{msg, I, _} when I < N ->
	    io:format("~p (Slave): Old sequence number detected (~p<~p)~n", [Id, I, N]),
	    slave(Id, Master, Leader, N, Last, Peers);
	{msg, N, Msg} ->
	    Master ! {deliver, Msg},
	    slave(Id, Master, Leader, N+1, {msg, N, Msg}, Peers);
	{view, N, State, Leader, View} ->
	    slave(Id, Master, Leader, N, Last, View);
	{'DOWN', _Ref, process, Leader, _Reason} ->
	    io:format("~p (Slave): Received DOWN~n", [Id]),
	    election(Id, Master, N, Last, Peers);
	stop ->
	    io:format("~p (Slave): Stopped~n", [Id]),
	    ok;
	Error ->
	    io:format("~p (Slave): Unknown message received: ~w~n", [Id, Error])
    end.

leader(Id, Master, N, Peers) ->
    receive
	{mcast, Msg} ->
	    bcast(Id, {msg, N, Msg}, Peers),
	    Master ! {deliver, Msg},
	    leader(Id, Master, N+1, Peers);
	{join, Peer} ->
	    Master ! request,
	    joining(Id, Master, N, Peer, Peers);
	stop ->
	    io:format("~p (Leader): Stopped~n", [Id]),
	    ok;
	Error ->
	    io:format("~p (Leader): Unknown message received: ~w~n", [Id, Error])
    end.

joining(Id, Master, N, Peer, Peers) ->
    io:format("~p (Leader): Joining...~n", [Id]),
    timer:sleep(50),
    receive
	{ok, State} ->
	    Peers2 = lists:append(Peers, [Peer]),
	    bcast(Id, {view, N, State, self(), Peers2}, Peers2),
	    leader(Id, Master, N, Peers2)
    end.

election(Id, Master, N, Last, [Leader|Rest]) ->
    io:format("~p: Electing new leader...~n", [Id]),
    if
	Leader == self() ->
	    register(leader, Leader),
	    bcast(Id, Last, Rest),
	    io:format("~p: I am the new leader!~n", [Id]),
	    leader(Id, Master, N, Rest);
	true ->
	    erlang:monitor(process, Leader),
	    io:format("~p: ~p is the new leader!~n", [Id, Leader]),
	    slave(Id, Master, Leader, N, Last, Rest)
    end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
    case random:uniform(?arghh) of
	?arghh ->
	    io:format("~p (Leader): Crashed!~n", [Id]),
	    exit(no_luck);
	_ ->
	    ok
    end.
