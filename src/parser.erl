-module(parser).
-compile([export_all]).
-define(FILE_NAME_PATTERN, ".{1,}\..{1,}").
-define(ATTACHMENT_FILE_NAME_PATTERN, "filename=\".*\"").
-define(HTTP_PATTERN, "^http://.*").
-define(HTTPS_PATTERN, "^https://.*").
-define(CONTENT_DISPOSITION, "content-disposition").

file_name_from_header(Headers) ->
  HeadersMap = maps:from_list(Headers),
  case maps:is_key(?CONTENT_DISPOSITION, HeadersMap) of
    true ->
      ContentDisposition = maps:get(?CONTENT_DISPOSITION, HeadersMap),
      Tokens = string:tokens(ContentDisposition, ";"),
      FileName = file_name(find_first_in_list("filename", Tokens)),
      {exist, FileName};
    false ->
      notexist
  end.


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

file_name(FileName) ->
  case re:run(FileName, ?ATTACHMENT_FILE_NAME_PATTERN) of
    {match, _} ->
      NameInScopes = get_second_from_list(string:tokens(FileName, "=")),
      NameWithOutScopes = string:substr(NameInScopes, 2, string:len(NameInScopes) - 2),
      NameWithOutScopes;
    nomatch ->
      ""
end.


get_first_from_list([H | _]) -> H.

get_second_from_list([_ | [V | _]]) -> V.

find_first_in_list(_, []) -> null;
find_first_in_list(Key, [H | Tail]) ->
  case string:str(H, Key) of
    0 ->
      find_first_in_list(Key, Tail);
    _ ->
      H
  end.