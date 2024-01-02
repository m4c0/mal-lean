-module(step2_eval).
-export([main/1, add/1, sub/1, mult/1, dv/1]).

add(_) -> {error, "TBD"}.
sub(_) -> {error, "TBD"}.
mult(_) -> {error, "TBD"}.
dv(_) -> {error, "TBD"}.

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
eval({list, []}, _) -> {list, []};
eval({list, L}, Env) ->
  case eval_ast({list, L}, Env) of
    {list, [{lambda, Fn}|NL]} -> Fn(NL);
    X -> X
  end;
eval({vector, V}, Env) -> eval_ast({vector, V}, Env);
eval({hashmap, V}, Env) -> eval_ast({hashmap, V}, Env);
eval(X, Env) -> eval_ast(X, Env).
print(X) -> printer:pr_str(X, true).

eval_ast({symbol, X}, Env) ->
  case maps:find(X, Env) of
    {ok, V} -> V;
    error -> {error, "unknown symbol"}
  end;
eval_ast({list, L}, Env) ->
  NL = lists:map(fun (V) -> eval(V, Env) end, L),
  {list, NL};
eval_ast({vector, L}, Env) ->
  NL = lists:map(fun (V) -> eval(V, Env) end, L),
  {vector, NL};
eval_ast({hashmap, M}, Env) ->
  NM = maps:map(fun (_, V) -> eval(V, Env) end, M),
  {hashmap, NM};
eval_ast(X, _) -> X.
