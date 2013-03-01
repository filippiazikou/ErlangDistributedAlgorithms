%% Author: filippia
%% Created: Oct 3, 2012
%% Description: TODO: Add description to test
-module(test2).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/1]).

start(N) ->
	FirstNode = node2:start(1),
	start_rest(N-1, FirstNode),
	FirstNode.
	
start_rest(0, _) ->
	ok;
start_rest(N, FirstNode) ->
	node2:start(N, FirstNode),
	start_rest(N-1, FirstNode).
