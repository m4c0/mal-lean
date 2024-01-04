-module(core).
-export([ns/0]).

ns() ->
  maps:map(fun (_, V) -> {lambda, V} end,
           #{"+" => fun add/1,
             "-" => fun sub/1,
             "*" => fun mult/1,
             "/" => fun dv/1,
             "<" => fun lt/1,
             ">" => fun gt/1,
             "<=" => fun lte/1,
             ">=" => fun gte/1,
             "=" => fun eq/1,
             "count" => fun count/1,
             "empty?" => fun empty/1,
             "list" => fun list/1,
             "list?" => fun listq/1,
             "println" => fun println/1,
             "prn" => fun prn/1,
             "pr-str" => fun prstr/1,
             "read-string" => fun readstring/1,
             "str" => fun str/1}).

add([{number, A},{number, B}]) -> {number, A+B};
add(_) -> {error, "invalid parameters"}.

sub([{number, A},{number, B}]) -> {number, A-B};
sub(_) -> {error, "invalid parameters"}.

mult([{number, A},{number, B}]) -> {number, A*B};
mult(_) -> {error, "invalid parameters"}.

dv([{number, A},{number, B}]) -> {number, A div B};
dv(_) -> {error, "invalid parameters"}.

lt([{number, A},{number, B}]) -> {boolean, A < B};
lt(_) -> {error, "invalid parameters"}.

gt([{number, A},{number, B}]) -> {boolean, A > B};
gt(_) -> {error, "invalid parameters"}.

lte([{number, A},{number, B}]) -> {boolean, A =< B};
lte(_) -> {error, "invalid parameters"}.

gte([{number, A},{number, B}]) -> {boolean, A >= B};
gte(_) -> {error, "invalid parameters"}.

eq([{seq, _, A},{seq, _, B}]) -> seq_eq(A, B);
eq([A,B]) -> {boolean, A == B};
eq(_) -> {error, "invalid parameters for ="}.

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

prstr(L) -> fmt(L, " ", true).

readstring([{string, X}]) -> reader:read_str(X);
readstring(_) -> {error, "invalid parameters for read-string"}.

str(L) -> fmt(L, "", false).

%% helpers

fmt(L, Sep, Fmt) ->
  NL = lists:map(fun (V) -> printer:pr_str(V, Fmt) end, L),
  {string, string:join(NL, Sep)}.

iofmt([], _, _) -> io:format("~n", []), nil;
iofmt([X|Xs], Sep, Fmt) -> 
  io:format("~s~s", [Sep, printer:pr_str(X, Fmt)]),
  iofmt(Xs, " ", Fmt).

seq_eq([], []) -> {boolean, true};
seq_eq([A|AA],[B|BB]) ->
  case eq([A, B]) of
    {boolean, true} -> seq_eq(AA, BB);
    X -> X
  end;
seq_eq(_, _) -> {boolean, false}.
