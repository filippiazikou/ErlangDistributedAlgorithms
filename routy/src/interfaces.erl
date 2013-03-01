-module(interfaces).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, broadcast/2]).

%%%return an empty set of interfaces.
new() ->
	[].

%%% add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf ) ->
	[{Name, Ref, Pid} | Intf].

%%% remove an entry given a name of an interface, return a new set of interfaces.
remove(Name, Intf ) ->
	lists:keydelete(Name, 1, Intf).

%%% find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf ) ->
	case lists:keyfind(Name, 1, Intf) of
		{N, R, P} ->
			{ok, P};
		false ->
			notfound
	end.
	
%%% find the reference given a name and return {ok, Ref} or notfound.
ref(Name, Intf ) ->
	case lists:keyfind(Name, 1, Intf) of
		{N, R, P} ->
			{ok, R};
		false ->
			notfound
	end.

%%% find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf ) ->
	case lists:keyfind(Ref, 2, Intf) of
		{N, R, P} ->
			{ok, N};
		false ->
			notfound
	end.

%%% return a list with all names.
list(Intf) -> 
	list(Intf, []).
list([], NameList) ->
	NameList;
list([H|T], NameList) ->
	{Name, Ref, Pid} = H,
	list(T, [Name|NameList]).

%%% send the message to all interface processes.
broadcast(Message, []) ->
	sent;
broadcast(Message, Intf ) ->
	[{N, R, P}|T] = Intf,
	P ! Message,
	broadcast(Message, T).
