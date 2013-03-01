-module(gmsa).

-export([start/1, start/2]).

-define(timeout, 2000).
%% Crasg propability 1/100 
-define(crash, 80).

%%%%%%%%%%%%%%%%%%%%%
%% Start the leader %
%%%%%%%%%%%%%%%%%%%%%
start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id, Rnd, Self) end).

init(Id, Rnd, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master, 0, [], [Master]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start slave to join s group %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id, Grp, Rnd, Self) end).

init(Id, Grp, Rnd, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	Self = self(),
	Grp ! {join, Master, Self},
	 
	receive
		{view, N, [Leader|Slaves], Group} ->
			Master ! {view, Group},
			%% The slave monitors the leader %% 			
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N, {null}, [Leader|Slaves], Group)
	after ?timeout ->
			Master ! {error, "no reply from leader"}

	end.


%%%%%%%%%%%%%%
%% The slave %
%%%%%%%%%%%%%%
slave(Id, Master, Leader, N, Last,  Slaves, Group) ->
	receive
		%% Forward multicast message to Leader %% 		
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		
		%% Request from a peer to join group  %%
		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			io:format("~p (Slave): Join peer ~p~n", [Id, Peer]),
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		
		%% Forward  message to Master %%
		{msg, N, Msg} ->
			Master ! Msg,
			slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group);
		%% Discard messages already seen 		
		{msg, I, _} when I < N ->
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		
		%% Receive view, forward to master %%
		{view, N, [Leader2|Slaves2], Group2} ->
			Master ! {view, Group2},
			slave(Id, Master, Leader2, N+1, {view, N, [Leader2|Slaves2], Group2}, Slaves2, Group2);
		
		%% The leader is down, initiate elections 		
		{'DOWN', _Ref, process, _Leader, _Reason} ->
		    io:format("~p: Slave: Received DOWN~n", [Id]),
			election(Id, Master, N, Last, Slaves, Group);
		stop ->
			ok
	end.

%%%%%%%%%%%%%%%
%% The leader %
%%%%%%%%%%%%%%%
leader(Id, Master, N, Slaves, Group) ->
	receive
		%% Receive message to broadcast %
		%% Inform the Master 			%	
		{mcast, Msg} ->
			bcast(Id, {msg, N, Msg}, Slaves),
			Master ! Msg,
			leader(Id, Master, N+1, Slaves, Group);

		%% Request from a peer to join group  %% 		
		{join, Wrk, Peer} ->
			Slaves2 = lists:append(Slaves, [Peer]),
			Group2 = lists:append(Group, [Wrk]),
			bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
			Master ! {view, Group2},
			leader(Id, Master, N+1, Slaves2, Group2);
		stop ->
			ok
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inform other nodes of msg %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bcast(_, Msg, Nodes) ->
%% 	lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

bcast(Id, Last, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Last, crash(Id) end, Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Election of a new leader if the old is dead %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
election(Id, Master, N, Last, Slaves, [_|Group]) ->
	Self = self(),
	case Slaves of
		[Self|Rest] ->
			bcast(Id, Last, Rest),
			bcast(Id, {view,N, Slaves, Group}, Rest),
			Master ! {view, Group},
			io:format("~p: New leader is me~n", [Id]),
			leader(Id, Master, N+1, Rest, Group);
		[Leader|Rest] ->
			io:format("~p: New leader , other.~n", [Id]),
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N, Last, Rest, Group)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Randomly crash the leader %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crash(Id) ->
	case random:uniform(?crash) of
		?crash ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ ->	
			ok
	end.
