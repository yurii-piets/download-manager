-module(manager).
-compile([export_all]).

start() ->
  Queue = queue:new(),
  spawn(manager, do, [Queue]).

do(Queue) ->
  receive
    {start, Link, Dir} ->
      io:format("~p~n", [Link]),
      io:format("~p~n", [Dir]);
    _ -> io:fwrite("Unknow command received in queue.")
  end,
  do(Queue).