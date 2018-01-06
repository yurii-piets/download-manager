-module(download).
-compile([export_all]).

start(Tuple, Manager) ->
  spawn(download, begin_download, [Tuple, Manager]).

begin_download({Link, Dir}, Manager) ->
  inets:start(),
  Response = httpc:request(get, {Link, []}, [], []),
  case Response of
    {ok, {{_, 200, "OK"}, _, Body}} ->
      save_body(),
      Manager ! {finish, {Link, Dir}};
    _ ->
      io:format("Unkreconbizable response for link ~p~n", [Link])
  end
.

save_body() ->
%  todo save body to file
  erlang:error(not_implemented).