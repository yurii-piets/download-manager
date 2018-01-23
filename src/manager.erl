-module(manager).
-export([start/0, manage/2]).

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
          NewQueue = queue:in({Link, Dir}, Queue),
          manage(NewQueue, Map)
      end;
    {stop, Link} ->
      ContainsLink = maps:is_key(Link, Map),
      if
        ContainsLink ->
          {_, Pid} = maps:get(Link, Map),
          NewMap = maps:remove(Link, Map),
          exit(Pid, "Download interupted."),
          download_from_queue(Queue, NewMap);
        true ->
          io:format("~nNo pending download for link.~nEnter command> ", []),
          manage(Queue, Map)
      end;
    {finish, {Link, _}} ->
      NewMap = maps:remove(Link, Map),
      io:format("~n[DONE] ~p~nEnter command> ", [Link]),
      download_from_queue(Queue, NewMap);
    list ->
      list_downloads(Map),
      manage(Queue, Map);
    queue ->
      list_queued_downloads(Queue),
      manage(Queue, Map);
    _ ->
      io:fwrite("~nUnknow command received in queue.~nEnter command> "),
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

list_downloads(Map) ->
  Keys = maps:keys(Map),
  if
    length(Keys) > 0 ->
      io:format("~n"),
      lists:foreach(
        fun(Item) ->
          io:format("~n [~p]~nEnter command> ", [Item]) end,
        Keys
      );
    true ->
      io:format("~nList of downloads is empty.~nEnter command> ")
  end.

list_queued_downloads(Queue) ->
  QueueIsEmpty = queue:is_empty(Queue),
  if
    QueueIsEmpty ->
      io:format("~nNo queued download.~nEnter command> ");
    true ->
      io:format("~n"),
      List = queue:to_list(Queue),
      lists:foreach(
        fun({Link, _}) ->
          io:format("[~p]~nEnter command> ", [Link]) end,
        List
      )
  end.
