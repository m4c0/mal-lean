-module(step3_env).
-export([main/1, add/1, sub/1, mult/1, dv/1]).

add([{number, A},{number, B}]) -> {number, A+B};
add(_) -> {error, "invalid parameters"}.

sub([{number, A},{number, B}]) -> {number, A-B};
sub(_) -> {error, "invalid parameters"}.

mult([{number, A},{number, B}]) -> {number, A*B};
mult(_) -> {error, "invalid parameters"}.

dv([{number, A},{number, B}]) -> {number, A div B};
dv(_) -> {error, "invalid parameters"}.

main(_) ->
  Env = env:new(nil),
  env:set(Env, "+", {lambda, fun step3_env:add/1}),
  env:set(Env, "-", {lambda, fun step3_env:sub/1}),
  env:set(Env, "*", {lambda, fun step3_env:mult/1}),
  env:set(Env, "/", {lambda, fun step3_env:dv/1}),
  repl(Env).

repl(Env) ->
  case io:get_line("user> ") of
    eof -> io:fwrite("eof");
    {error, X} -> io:fwrite("error: ~s", [X]);
    Line ->
      io:nl(), %% Required on Windows
      io:fwrite("~s", [rep(Line, Env)]),
      io:nl(),
      repl(Env)
  end.

rep(X, Env) -> print(eval(read(X), Env)).

read(X) -> reader:read_str(X).

eval({list, []}, _) -> {list, []};
eval({list, [{symbol, "def!"},{symbol, K},V]}, Env) ->
  VV = eval_ast(V, Env),
  env:set(Env, K, VV),
  VV;
eval({list, [{symbol, "def!"}|_]}, _) ->
  {error, "invalid def! signature"};
eval({list, [{symbol, "let*"}|_]}, _) ->
  {error, "invalid let* signature"};
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
  case env:get(Env, X) of
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
