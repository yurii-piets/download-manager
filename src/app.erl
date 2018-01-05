-module(app).
-compile([export_all]).

main() ->
  Manager = manager:start(),
  console:start(Manager).
