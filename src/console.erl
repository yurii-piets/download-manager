-module(console).
-compile([export_all]).

-define(START_PATTERN, "start -l .* -d .*").
-define(STOP_PATTERN, "stop -l .*").

start(Manager) ->
  do(Manager).

do(Manager) ->
  Line = io:get_line("Enter command> "),
  process(string:trim(Line), Manager),
  do(Manager).

process(Line, Manager) ->
  Type = parseType(Line),
  case Type of
    start ->
      Link = parseLink(Line),
      Dir = parseDir(Line),
      Manager ! {start, {Link, Dir}};
    stop ->
      Link = parseLink(Line),
      Manager ! {stop, Link};
    _ ->
      io:fwrite("Wrong command")
  end.

parseType(Line) ->
  MatchesStart = matches_pattern(Line, ?START_PATTERN),
  MatchesStop = matches_pattern(Line, ?STOP_PATTERN),
  if
    MatchesStart == true ->
      start;
    MatchesStop == true ->
      stop;
    true ->
      unknown
  end.

parseLink(Line) ->
  StartIndex = string:str(Line, "-l ") + 3,
  LastIndex = string:str(Line, " -d") - 1,
  if
    StartIndex < LastIndex ->
      string:trim(string:sub_string(Line, StartIndex, LastIndex));
    LastIndex < 0 ->
      NewLastIndex = string:len(Line),
      string:trim(string:sub_string(Line, StartIndex, NewLastIndex));
    true ->
      io:fwrite("Link cannot be empty")
  end.

parseDir(Line) ->
  StartIndex = string:str(Line, "-d ") + 3,
  LastIndex = string:length(Line) - 1,
  if
    StartIndex < LastIndex ->
      string:trim(string:sub_string(Line, StartIndex, LastIndex));
    true ->
      io:fwrite("Dir cannot be empty")
  end.

matches_pattern(Line, Pattern) ->
  case re:run(Line, Pattern) of
    {match, _} ->
      true;
    nomatch ->
      false
  end.
