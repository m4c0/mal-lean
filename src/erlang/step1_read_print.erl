-module(step1_read_print).
-import(reader, []).
-export([main/1]).

main(A) ->
  case io:get_line("user> ") of
    eof -> io:fwrite("eof");
    {error, _} -> io:fwrite("error somewhere");
    Line ->
      io:nl(), %% Required on Windows
      io:fwrite("~s", [rep(string:chomp(Line))]),
      io:nl(),
      main(A)
  end.

rep(X) -> print(eval(read(X))).

read(X) -> X.
eval(X) -> X.
print(X) -> X.
           
