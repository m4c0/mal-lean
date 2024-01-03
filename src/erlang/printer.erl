-module(printer).
-export([pr_str/2]).

pr_str({error, E}, _) -> "error: " ++ E;
pr_str({lambda, _}, _) -> "#<function>";
pr_str(nil, _) -> "nil";
pr_str({boolean, true}, _) -> "true";
pr_str({boolean, false}, _) -> "false";
pr_str({symbol, S}, _) -> S;
pr_str({keyword, S}, _) -> S;
pr_str({number, N}, _) -> integer_to_list(N);
pr_str({string, S}, false) -> S;
pr_str({string, S}, true) -> print_str(S);
pr_str({list, L}, R) -> "(" ++ print_list(L, R) ++ ")";
pr_str({vector, L}, R) -> "[" ++ print_list(L, R) ++ "]";
pr_str({hashmap, L}, R) -> "{" ++ print_map(L, R) ++ "}";
pr_str(X, _) -> io_lib:format("error: unknown result~n       ~p", [X]).

print_list(L, R) -> string:join(lists:map(fun (S) -> pr_str(S, R) end, L), " ").

print_map(M, R) ->
  Fn = fun (K, V, Acc) -> [K,V|Acc] end,
  L = maps:fold(Fn, [], M),
  print_list(L, R).

print_str(S) -> lists:reverse(print_str(S, [$"])).
print_str([], Acc) -> [$"|Acc];
print_str([$\\|S], Acc) -> print_str(S, "\\\\" ++ Acc);
print_str([$\n|S], Acc) -> print_str(S, "n\\" ++ Acc);
print_str([$"|S], Acc) -> print_str(S, "\"\\" ++ Acc);
print_str([C|S], Acc) -> print_str(S, [C|Acc]).
