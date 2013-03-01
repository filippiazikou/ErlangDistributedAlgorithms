-module(script).

-export([start/4]).

start(N, Module, Rand, Wait) ->
	Leader = worker:start(leader, Module, Rand, Wait),
	slave_start(N-1, Module, Rand, Wait, Leader).

slave_start(0, _, _, _, _) ->
	ok;
slave_start(N, Module, Rand, Wait, Leader) ->
	timer:sleep(500),
	worker:start(list_to_atom(integer_to_list(N) ++ "Slave"), Module, Rand, Leader, Wait),
	slave_start(N-1, Module, Rand, Wait, Leader).
	
