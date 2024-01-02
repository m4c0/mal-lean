-module(step1_read_print).
-export([main/1]).

main(A) ->
  case io:get_line("user> ") of
    eof -> io:fwrite("eof");
    {error, X} -> io:fwrite("error: ~s", [X]);
    Line ->
      io:nl(), %% Required on Windows
      io:fwrite("~s", [rep(Line)]),
      io:nl(),
      main(A)
  end.

rep(X) -> print(eval(read(X))).

read(X) -> reader:read_str(X).
eval(X) -> X.
print(X) -> printer:pr_str(X).
