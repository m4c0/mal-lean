-module(core).
-export([ns/0]).

ns() ->
  maps:map(fun (_, V) -> {lambda, V} end,
           #{"+" => fun add/1,
             "-" => fun sub/1,
             "*" => fun mult/1,
             "/" => fun dv/1,
             "count" => fun count/1,
             "empty?" => fun empty/1,
             "list" => fun list/1,
             "list?" => fun listq/1,
             "println" => fun println/1,
             "prn" => fun prn/1}).

add([{number, A},{number, B}]) -> {number, A+B};
add(_) -> {error, "invalid parameters"}.

sub([{number, A},{number, B}]) -> {number, A-B};
sub(_) -> {error, "invalid parameters"}.

mult([{number, A},{number, B}]) -> {number, A*B};
mult(_) -> {error, "invalid parameters"}.

dv([{number, A},{number, B}]) -> {number, A div B};
dv(_) -> {error, "invalid parameters"}.

count([{seq, _, L}]) -> {number, length(L)};
count([nil]) -> {number, 0};
count(_) -> {error, "invalid parameters for count"}.

empty([{seq, _, []}]) -> {boolean, true};
empty([_]) -> {boolean, false};
empty(_) -> {error, "invalid parameters for empty?"}.

list(X) -> {seq, list, X}.

listq([{seq, list, _}]) -> {boolean, true};
listq([_]) -> {boolean, false};
listq(_) -> {error, "invalid parameters for list?"}.

println(L) -> iofmt(L, "", false).
prn(L) -> iofmt(L, "", true).

iofmt([], _, _) -> io:format("~n", []), nil;
iofmt([X|Xs], Sep, Fmt) -> 
  io:format("~s~s", [Sep, printer:pr_str(X, Fmt)]),
  iofmt(Xs, " ", Fmt).
