-module(parser).
-compile([export_all]).
-define(FILE_NAME_PATTERN, ".{1,}\..{1,}").
-define(HTTP_PATTERN, "^http://.*").
-define(HTTPS_PATTERN, "^https://.*").
-define(CONTENT_DISPOSITION, "content-disposition").

file_name_from_header(Headers) ->
  ContentDisposition = maps:get(?CONTENT_DISPOSITION, maps:from_list(Headers)),
  ContentDisposition.

file_name_from_link(Link) ->
  LinkWithoutParameters = get_first_from_list(string:tokens(Link, "?")),
  Tokens = string:tokens(LinkWithoutParameters, "/"),
  Name = lists:last(Tokens),

  case re:run(Name, ?FILE_NAME_PATTERN) of
    {match, _} ->
      {extentention, Name};
    nomatch ->
      {noextentention, Name}
  end.

get_first_from_list([H | _]) -> H.