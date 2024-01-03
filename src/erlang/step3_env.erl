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

%% read-eval-print

rep(X, Env) -> print(eval(read(X), Env)).

read(X) -> reader:read_str(X).

eval({seq, list, []}, _) -> {seq, list, []};
eval({seq, list, [{symbol, "def!"},{symbol, K},V]}, Env) ->
  case eval(V, Env) of
    {error, X} -> {error, X};
    VV -> env:set(Env, K, VV), VV
  end;
eval({seq, list, [{symbol, "def!"}|_]}, _) ->
  {error, "invalid def! signature"};
eval({seq, list, [{symbol, "let*"},{seq, _, As},P]}, Env) ->
  NEnv = env:new(Env),
  case bind(As, NEnv) of
    ok -> eval(P, NEnv);
    X -> X
  end;
eval({seq, list, [{symbol, "let*"}|_]}, _) ->
  {error, "invalid let* signature"};
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

%% eval_ast

eval_ast({symbol, X}, Env) ->
  case env:get(Env, X) of
    {ok, V} -> V;
    error -> {error, io_lib:format("~s not found", [X])}
  end;
eval_ast({seq, Seq, L}, Env) ->
  Fn = fun (V, Acc) when is_list(Acc) ->
           case eval(V, Env) of
             {error, X} -> {error, X};
             X -> [X|Acc]
           end;
           (_, X) -> X
       end,
  case lists:foldl(Fn, [], L) of
    NL when is_list(NL) -> {seq, Seq, lists:reverse(NL)};
    X -> X
  end;
eval_ast({hashmap, M}, Env) ->
  Fn = fun (K, V, Acc) when is_map(Acc) ->
           case eval(V, Env) of
             {error, X} -> {error, X};
             X -> Acc#{K => X}
           end;
           (_, _, X) -> X
       end,
  case maps:fold(Fn, #{}, M) of
    NM when is_map(NM) -> {hashmap, NM};
    X -> X
  end;
eval_ast(X, _) -> X.

%% others

bind([], _) -> ok;
bind([{symbol, K},V|L], Env) ->
  case eval(V, Env) of
    {error, X} -> {error, X};
    VV -> env:set(Env, K, VV),
          bind(L, Env)
  end;
bind(_, _) -> {error, "invalid parameter pair"}.

