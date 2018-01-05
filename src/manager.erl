-module(manager).
-compile([export_all]).
-define(MAX_DOWNLOADS, 8).

start() ->
  Queue = queue:new(),
  List = [],
  spawn(manager, manage, [Queue, List]).

manage(Queue, List) ->
  receive
    {start, Tuple} ->
      ListLength = length(List),
      if
        ListLength < ?MAX_DOWNLOADS ->
          start_download(Tuple),
          NewList = lists:append([Tuple], List),
          manage(Queue, NewList);
        true ->
          NewQueue = queue:in(Queue, Tuple),
          manage(NewQueue, List)
      end;
    {finish, Tuple} ->
      NewList = lists:delete(List, Tuple),

      case queue:out(Queue) of
        {{value, Item}, NewQueue} ->
          start_download(Tuple),
          NewList = lists:append(List, Item),
          manage(NewQueue, NewList);
        _ ->
          manage(Queue, NewList)
      end;
    _ ->
      io:fwrite("Unknow command received in queue."),
      manage(Queue, List)
  end.


start_download({Link, Dir}) ->
  io:format("Dowloading ~p~n~p~n", [Link, Dir]).
