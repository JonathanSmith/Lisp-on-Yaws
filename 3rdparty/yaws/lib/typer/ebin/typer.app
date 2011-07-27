% This is an -*- erlang -*- file.

{application, typer,
 [{description, "TYPe annotator for ERlang programs, version 0.9.1"},
  {vsn, "0.9.1"},
  {modules, [typer]},
  {registered, []},
  {applications, [compiler, dialyzer, hipe, kernel, stdlib]},
  {env, []}]}.
