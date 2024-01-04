-module(macro).
-export([expand/2]).

expand({seq, list, [{symbol, K}|As]} = X, Env) ->
  case env:get(Env, K) of
    {ok, {macro, Fn}} -> expand(Fn(As), Env);
    _ -> X
  end;
expand(X, _) -> X.
