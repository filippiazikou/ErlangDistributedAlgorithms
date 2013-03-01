-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
	spawn(fun() ->init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) ->
 	loop(init_list(Nodes, []), []).

init_list([], WorkerTimes) ->
	WorkerTimes;
init_list([H|T], WorkerTimes) ->
 	init_list(T, [{H, 0} | WorkerTimes]).
	

loop(WorkerTimes, Queue) ->
	receive
		{log, From, Time, Msg} ->
			%Get the minimum internal time
			NewQueue = lists:keysort(2, [{From, Time, Msg} | Queue]),
			NewWorkerTimes = lists:keysort(2,  lists:keystore(From, 1, WorkerTimes, {From, Time})),
			[{Worker, Min}|T] = NewWorkerTimes,
			{ForPrint, NewNewQueue} = lists:splitwith(fun({_,T, _}) -> T<Min end, NewQueue),
			print(ForPrint),
			loop(NewWorkerTimes, NewNewQueue);
		stop ->
			print(Queue),
			ok
	end.
	
log(From, Time, Msg) ->
	io:format("log ~p~n", [From, Time, Msg]).

 print(List) ->
 	case List of 
 		[] ->
 			ok;
		[H|T] ->
 			io:format("~w~n", [H]),
 			print(T)
 	end.

printSender(List) ->
 	case List of 
 		[] ->
 			ok;
		[H|T] ->
			case H of 
				{_, _, {sending, _}} ->
					io:format("~w~n", [H]),
 					print(T);
				_ ->
 					print(T)
			end
 	end.