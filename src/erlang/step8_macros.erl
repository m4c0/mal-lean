-module(step8_macros).
-export([main/1]).

main(As) ->
  Env = env:new(nil),
  maps:foreach(fun (K, V) -> env:set(Env, K, V) end, core:ns()),
  rep("(def! not (fn* (a) (if a false true)))", Env),
  rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))", Env),
  env:set(Env, "eval", {lambda, fun (X) -> eval_eval(X, Env) end}),
  case As of
    [Fn|AA] -> 
      MA = lists:map(fun (A) -> {string, A} end, AA),
      env:set(Env, "*ARGV*", {seq, list, MA}),
      rep("(load-file \"" ++ Fn ++ "\")", Env);
    [] ->
      env:set(Env, "*ARGV*", {seq, list, []}),
      repl(Env)
  end.

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

eval({seq, list, V}, Env) -> eval_list(V, Env);
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
             {error, _} = X -> X;
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
             {error, _} = X -> X;
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

%% eval bits

eval_list([], _) -> {seq, list, []};
eval_list([{symbol, "quote"},Ast],_) -> Ast;
eval_list([{symbol, "quasiquoteexpand"},Ast],_) -> quasiquote:expand(Ast);
eval_list([{symbol, "quasiquote"},Ast],Env) ->
  Exp = quasiquote:expand(Ast),
  eval(Exp, Env);
eval_list([{symbol, "do"}|L], Env) -> eval_do(L, Env);
eval_list([{symbol, "if"},Cond,T], Env) -> eval_if(Cond, T, nil, Env);
eval_list([{symbol, "if"},Cond,T,F], Env) -> eval_if(Cond, T, F, Env);
eval_list([{symbol, "fn*"},{seq,_,Binds},Body], Env) ->
  {lambda, fun (Exprs) -> 
               case env:new(Env, Binds, Exprs) of
                 {error, X} -> {error, X};
                 NEnv -> eval(Body, NEnv)
               end
           end};
eval_list([{symbol, "fn*"}|_], _) ->
  {error, "invalid fn* signature"};
eval_list([{symbol, "def!"},{symbol, K},V], Env) ->
  case eval(V, Env) of
    {error, X} -> {error, X};
    VV -> env:set(Env, K, VV), VV
  end;
eval_list([{symbol, "def!"}|_], _) ->
  {error, "invalid def! signature"};
eval_list([{symbol, "defmacro!"},{symbol, K},V], Env) ->
  case eval(V, Env) of
    {error, X} -> {error, X};
    {lambda, L} -> VV = {macro, L}, env:set(Env, K, VV), VV
  end;
eval_list([{symbol, "defmacro!"}|_], _) ->
  {error, "invalid def! signature"};
eval_list([{symbol, "macroexpand"},E], Env) -> macro:expand(E, Env);
eval_list([{symbol, "let*"},{seq,_,As},P], Env) ->
  NEnv = env:new(Env),
  case bind(As, NEnv) of
    ok -> eval(P, NEnv);
    X -> X
  end;
eval_list([{symbol, "let*"}|_], _) ->
  {error, "invalid let* signature"};
eval_list(L, Env) ->
  case eval_ast({seq, list, L}, Env) of
    {seq, list, [{lambda, Fn}|NL]} -> Fn(NL);
    X -> X
  end.

eval_if(Cond, T, F, Env) ->
  case eval(Cond, Env) of
    {boolean, false} -> eval(F, Env);
    nil -> eval(F, Env);
    {error, X} -> {error, X};
    _ -> eval(T, Env)
  end.

eval_do([], _) -> {error, "do'ng an empty list"};
eval_do([E], Env) -> eval(E, Env);
eval_do([E|L], Env) ->
  case eval(E, Env) of
    {error, X} -> {error, X};
    _ -> eval_do(L, Env)
  end.

eval_eval([E], Env) -> eval(E, Env);
eval_eval(_, _) -> {error, "invalid eval signature"}.
