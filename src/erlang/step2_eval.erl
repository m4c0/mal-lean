-module(step2_eval).
-export([main/1, add/1, sub/1, mult/1, dv/1]).

add([{number, A},{number, B}]) -> {number, A+B};
add(_) -> {error, "invalid parameters"}.

sub([{number, A},{number, B}]) -> {number, A-B};
sub(_) -> {error, "invalid parameters"}.

mult([{number, A},{number, B}]) -> {number, A*B};
mult(_) -> {error, "invalid parameters"}.

dv([{number, A},{number, B}]) -> {number, A div B};
dv(_) -> {error, "invalid parameters"}.

main(A) ->
  Env = #{"+" => {lambda, fun step2_eval:add/1},
          "-" => {lambda, fun step2_eval:sub/1},
          "*" => {lambda, fun step2_eval:mult/1},
          "/" => {lambda, fun step2_eval:dv/1}},
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
eval({seq, list, []}, _) -> {seq, list, []};
eval({seq, list, L}, Env) ->
  case eval_ast({seq, list, L}, Env) of
    {seq, list, [{lambda, Fn}|NL]} -> Fn(NL);
    {seq, list, _} -> {error, "lists must start with a lambda to be eval'd"};
    X -> X
  end;
eval({seq, vector, V}, Env) -> eval_ast({seq, vector, V}, Env);
eval({hashmap, V}, Env) -> eval_ast({hashmap, V}, Env);
eval(X, Env) -> eval_ast(X, Env).
print(X) -> printer:pr_str(X, true).

eval_ast({symbol, X}, Env) ->
  case maps:find(X, Env) of
    {ok, V} -> V;
    error -> {error, "unknown symbol"}
  end;
eval_ast({seq, T, L}, Env) ->
  NL = lists:map(fun (V) -> eval(V, Env) end, L),
  {seq, T, NL};
eval_ast({hashmap, M}, Env) ->
  NM = maps:map(fun (_, V) -> eval(V, Env) end, M),
  {hashmap, NM};
eval_ast(X, _) -> X.
