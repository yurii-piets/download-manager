-module(app).
-export([main/0]).

main() ->
  Manager = manager:start(),
  console:start(Manager).
