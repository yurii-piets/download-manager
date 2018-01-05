-module(console).
-compile([export_all]).

-define(START_PATTERN, "start -l .* -d .*").

start() ->
  do().

do() ->
  Line = io:get_line("Enter command: "),
  Type = parseType(Line),
  io:fwrite(Type),
  io:fwrite("~n"),
  do().

parseType(Line) ->
  MatchesStart = matches_start(Line),
  if
    true == MatchesStart -> start;
    true -> unknown
  end.

matches_start(Line) ->
  case re:run(Line, ?START_PATTERN) of
    {match, _} -> true;
    nomatch -> false
  end.
