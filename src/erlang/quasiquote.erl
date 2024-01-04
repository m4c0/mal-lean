-module(quasiquote).
-export([expand/1]).

expand({seq, list, [{symbol, "unquote"}, E]}) -> E;
expand({T, _} = X) when T == hashmap; T == symbol ->
  {seq, list, [{symbol, "quote"}, X]};
expand(X) -> X.
