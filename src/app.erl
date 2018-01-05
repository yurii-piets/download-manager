-module(app).
-compile([export_all]).

main() ->
  Manager = spawn(manager, start, []),
  console:start(Manager).
