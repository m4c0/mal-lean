-module(step4_if_fn_do).
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
  env:set(Env, "+", {lambda, fun step4_if_fn_do:add/1}),
  env:set(Env, "-", {lambda, fun step4_if_fn_do:sub/1}),
  env:set(Env, "*", {lambda, fun step4_if_fn_do:mult/1}),
  env:set(Env, "/", {lambda, fun step4_if_fn_do:dv/1}),
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

eval({list, []}, _) -> {list, []};
eval({list, [{symbol, "do"}|L]}, Env) ->
  case eval_ast({list, L}, Env) of
    {list, NL} -> lists:last(NL);
    X -> X
  end;
eval({list, [{symbol, "if"},Cond,T]}, Env) -> eval_if(Cond, T, {nil, nil}, Env);
eval({list, [{symbol, "if"},Cond,T,F]}, Env) -> eval_if(Cond, T, F, Env);
eval({list, [{symbol, "def!"},{symbol, K},V]}, Env) ->
  case eval(V, Env) of
    {error, X} -> {error, X};
    VV -> env:set(Env, K, VV), VV
  end;
eval({list, [{symbol, "def!"}|_]}, _) ->
  {error, "invalid def! signature"};
eval({list, [{symbol, "let*"},{Seq, As},P]}, Env) when Seq == list; Seq == vector ->
  NEnv = env:new(Env),
  case bind(As, NEnv) of
    ok -> eval(P, NEnv);
    X -> X
  end;
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

%% eval_ast

eval_ast({symbol, X}, Env) ->
  case env:get(Env, X) of
    {ok, V} -> V;
    error -> {error, io_lib:format("~s not found", [X])}
  end;
eval_ast({Seq, L}, Env) when Seq == list; Seq == vector ->
  Fn = fun (V, Acc) when is_list(Acc) ->
           case eval(V, Env) of
             {error, X} -> {error, X};
             X -> [X|Acc]
           end;
           (_, X) -> X
       end,
  case lists:foldl(Fn, [], L) of
    NL when is_list(NL) -> {Seq, lists:reverse(NL)};
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

eval_if(Cond, T, F, Env) ->
  case eval(Cond, Env) of
    {boolean, false} -> eval(F, Env);
    {nil, _} -> eval(F, Env);
    {error, X} -> {error, X};
    _ -> eval(T, Env)
  end.
