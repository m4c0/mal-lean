-module(core).
-export([ns/0]).

ns() ->
  maps:map(fun (_, V) -> {lambda, V} end,
           #{"+" => fun add/1,
             "-" => fun sub/1,
             "*" => fun mult/1,
             "/" => fun dv/1,
             "prn" => fun prn/1}).

add([{number, A},{number, B}]) -> {number, A+B};
add(_) -> {error, "invalid parameters"}.

sub([{number, A},{number, B}]) -> {number, A-B};
sub(_) -> {error, "invalid parameters"}.

mult([{number, A},{number, B}]) -> {number, A*B};
mult(_) -> {error, "invalid parameters"}.

dv([{number, A},{number, B}]) -> {number, A div B};
dv(_) -> {error, "invalid parameters"}.

prn([]) -> {nil, nil};
prn([X|Xs]) -> 
  io:format("~s~n", [printer:pr_str(X, false)]),
  prn(Xs).
