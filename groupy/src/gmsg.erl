-module(gmsg).

-export([start/1, start/2]).
-define(timeout,500).
-define(arghh, 10000).


start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    spawn_link(fun()-> init(Id, Rnd, Self) end).

%%Initialisation of the Leader:
init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 1, [], [Master]).

start(Id, Grp) ->
    Self = self(),
    Rnd = random:uniform(1000),
    spawn_link(fun()-> init(Id, Grp, Rnd, Self) end).

%%Initialisation of the slaves:
init(Id, Grp, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader),
            Master ! {view, Group},
            N = 1,
            Last = "",
            slave(Id, Master, Leader, N, Last, Slaves, Group)
    after ?timeout ->
            Master ! {error, "No reply from Leader"}
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, I, _} when I < N ->
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, _, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, N + 1, Msg, Slaves, Group);
        {view, _, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, N, Last, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group);
        stop ->
            ok
    end.

leader(Id, Master, N, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N,[self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2);
        stop ->
            ok
    end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            bcast(Id, Last, Rest),
            leader(Id, Master, N, Rest, Group);
        [Leader|Rest] ->
            bcast(Id, Last, Rest),
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
        end.

crash(Id) ->
    case random:uniform(?arghh) of 
        ?arghh ->
            io:format("Leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ -> 
            ok
    end.