-module(quasiquote).
-export([expand/1]).

expand({seq, list, [{symbol, "unquote"}, E]}) -> E;
expand({seq, list, L}) -> qq(L);
expand({T, _} = X) when T == hashmap; T == symbol ->
  {seq, list, [{symbol, "quote"}, X]};
expand(X) -> X.

%% This breaks TCO, but it is required to pass step-7 tests.
%% Maybe there is a way to accumulate this in a TCO-friendly way, but the logic
%% is already too confusing as-is
qq([]) -> {seq, list, []};
qq([{seq, list, [{symbol, "splice-unquote"},E]}|L]) -> 
  {seq, list, [{symbol, "concat"},E,qq(L)]};
qq([E|L]) ->
  {seq, list, [{symbol, "cons"},expand(E),qq(L)]}.
