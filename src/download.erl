-module(download).
-compile([export_all]).
-define(CONTENT_TYPE, "content-type").

start(Tuple, Manager) ->
  spawn(download, begin_download, [Tuple, Manager]).

begin_download({Link, Dir}, Manager) ->
  inets_start_for_method(Link),
  Response = httpc:request(get, {Link, []}, [], []),
  case Response of
    {ok, {{_, 200, "OK"}, Headers, Body}} ->
      FileName = create_file_name(Link, Headers),
      save_file(Body, Dir, FileName),
      Manager ! {finish, {Link, Dir}},
      inets:stop();
    _ ->
      io:format("Unreconbizable response for link ~p~n", [Link])
  end.

save_file(Content, Dir, FileName) ->
  Path = Dir ++ FileName,
  file:write_file(Path, Content).

create_file_name(Link, Headers) ->
  case parser:file_name_from_link(Link) of
    {extentention, Name} ->
      Name;
    {noextentention, Name} ->
      ContentType = maps:get(?CONTENT_TYPE, maps:from_list(Headers)),
      Extension = converter:content_type_to_file_extension(ContentType),
      Name ++ Extension
  end.

inets_start_for_method(Link) ->
  {ok, {Method, _, _, _, _, _}} = http_uri:parse(Link),
  case Method of
    http ->
      inets:start();
    https ->
      ssl:start(),
      inets:start()
  end.
