-module(quasiquote).
-export([expand/1]).

expand({seq, list, [{symbol, "unquote"}, E]}) -> E;
expand({seq, list, L}) -> qq(L, []);
expand({T, _} = X) when T == hashmap; T == symbol ->
  {seq, list, [{symbol, "quote"}, X]};
expand(X) -> X.

qq([], Acc) -> {seq, list, []};
qq([{seq, list, [{symbol, "splice-unquote"},E]}|L], Acc) -> 
  {seq, list, [{symbol, "concat"},E,qq(L, Acc)]};
qq([E|L], Acc) ->
  {seq, list, [{symbol, "cons"},expand(E),qq(L, Acc)]}.
