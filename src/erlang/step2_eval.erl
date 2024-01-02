-module(step2_eval).
-export([main/1, add/1, sub/1, mult/1, dv/1]).

add(_) -> {error, "TBD"}.
sub(_) -> {error, "TBD"}.
mult(_) -> {error, "TBD"}.
dv(_) -> {error, "TBD"}.

main(A) ->
  Env = #{"+" => fun add/1,
          "-" => fun sub/1,
          "*" => fun mult/1,
          "/" => fun dv/1},
  case io:get_line("user> ") of
    eof -> io:fwrite("eof");
    {error, X} -> io:fwrite("error: ~s", [X]);
    Line ->
      io:nl(), %% Required on Windows
      io:fwrite("~s", [rep(Line, Env)]),
      io:nl(),
      main(A)
  end.

rep(X, Env) -> print(eval(read(X), Env)).

read(X) -> reader:read_str(X).
eval(X, Env) -> X.
print(X) -> printer:pr_str(X, true).
