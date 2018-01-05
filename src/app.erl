-module(app).
-compile([export_all]).

main() ->
  Manger = spawn(manager, start, []),
  shell:start(Manger).
