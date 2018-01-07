-module(manager).
-compile([export_all]).
-define(MAX_DOWNLOADS, 8).

start() ->
  Queue = queue:new(),
  List = maps:new(),
  spawn(manager, manage, [Queue, List]).

manage(Queue, Map) ->
  receive
    {start, {Link, Dir}} ->
      MapSize = maps:size(Map),
      if
        MapSize < ?MAX_DOWNLOADS ->
          DownloadPid = start_download({Link, Dir}),
          NewMap = maps:put(Link, {Dir, DownloadPid}, Map),
          manage(Queue, NewMap);
        true ->
          NewQueue = queue:in(Queue, {Link, Dir}),
          manage(NewQueue, Map)
      end;
    {stop, Link} ->
      ContainsLink = maps:is_key(Link, Map),
      if
        ContainsLink ->
          {Dir, Pid} = maps:get(Link, Map),
          NewMap = maps:remove(Link, Map),
          exit(Pid, "Download interupted."),
          download_from_queue(Queue, NewMap);
        true ->
          io:format("~nNo pending download for link.~n", []),
          manage(Queue, Map)
      end;
    {finish, {Link, Dir}} ->
      NewMap = maps:remove(Link, Map),
      io:format("~n[DONE] ~p~n", [Link]),
      download_from_queue(Queue, NewMap);
    _ ->
      io:fwrite("Unknow command received in queue."),
      manage(Queue, Map)
  end.

download_from_queue(Queue, List) ->
  case queue:out(Queue) of
    {{value, Item}, NewQueue} ->
      start_download(Item),
      NewList = lists:append(List, Item),
      manage(NewQueue, NewList);
    _ ->
      manage(Queue, List)
  end.

start_download(Tuple) ->
  download:start(Tuple, self()).
