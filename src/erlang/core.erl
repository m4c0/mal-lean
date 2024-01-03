-module(core).
-export([ns/0]).

ns() ->
  #{"+" => {lambda, fun add/1},
    "-" => {lambda, fun sub/1},
    "*" => {lambda, fun mult/1},
    "/" => {lambda, fun dv/1}}.

add([{number, A},{number, B}]) -> {number, A+B};
add(_) -> {error, "invalid parameters"}.

sub([{number, A},{number, B}]) -> {number, A-B};
sub(_) -> {error, "invalid parameters"}.

mult([{number, A},{number, B}]) -> {number, A*B};
mult(_) -> {error, "invalid parameters"}.

dv([{number, A},{number, B}]) -> {number, A div B};
dv(_) -> {error, "invalid parameters"}.
