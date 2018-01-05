-module(console).
-compile([export_all]).

-define(START_PATTERN, "start -l .* -d .*").

start(Manager) ->
  do(Manager).

do(Manager) ->
  Line = io:get_line("Enter command> "),
  process(Line, Manager),
  do(Manager).

process(Line, Manager) ->
  Type = parseType(Line),
  case Type of
    start ->
      Link = parseLink(Line),
      Dir = parseDir(Line),
      Manager ! {Type, Link, Dir};
    _ -> io:fwrite("Wrong command")
  end.

parseType(Line) ->
  MatchesStart = matches_start(Line),
  if
    true == MatchesStart -> start;
    true -> unknown
  end.

parseLink(Line) ->
  StartIndex = string:str(Line, "-l ") + 3,
  LastIndex = string:str(Line, " -d") - 1,
  if
    StartIndex < LastIndex -> string:sub_string(Line, StartIndex, LastIndex);
    true -> io:fwrite("Link cannot be empty")
  end.

parseDir(Line) ->
  StartIndex = string:str(Line, "-d ") + 3,
  LastIndex = string:length(Line) - 1,
  if
    StartIndex < LastIndex -> string:sub_string(Line, StartIndex, LastIndex);
    true -> io:fwrite("Dir cannot be empty")
  end.

matches_start(Line) ->
  case re:run(Line, ?START_PATTERN) of
    {match, _} -> true;
    nomatch -> false
  end.
