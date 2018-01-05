-module(manager).
-compile([export_all]).

start() ->
  Queue = queue:new(),
  spawn(manager, queue, [Queue]).

queue(Queue) ->
  receive
    {start, Link, Dir} ->
      NewQueue = Queue:in({Link, Dir}),
      queue(NewQueue);
    _ ->
      io:fwrite("Unknow command received in queue."),
      queue(Queue)
  end.
