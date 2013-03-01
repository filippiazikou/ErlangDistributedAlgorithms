-module(gui).

-define(width, 200).
-define(height, 200).
-define(bg, black).

-export([start/2]).

start(Id, Master) ->
	spawn_link(fun() -> init(Id, Master) end).

init(Id, Master) ->
	Win = gs:window(gs:start(),[{map,true},{title, Id}, {bg, ?bg},{width,?width},{height,?height}]),
	loop(Win, Master).

loop(Win, Master)->
	receive
		{color, Color} ->
			color(Win, Color),
			loop(Win, Master);
		{gs,_,destroy,[],[]} ->
			Master ! stop,
			ok;
		stop ->
			ok;
		Error ->
			io:format("gui: strange message ~w ~n", [Error]),
			loop(Win, Master)
	end.

color(Win, Color) ->
	gs:config(Win, [{bg, Color}]).