-module(printer).
-export([pr_str/2]).

pr_str({error, E}, _) -> "error: " ++ E;
pr_str({symbol, S}, _) -> S;
pr_str({number, N}, _) -> N;
pr_str({string, S}, false) -> S;
pr_str({string, S}, true) -> print_str(S);
pr_str({list, L}, R) -> "(" ++ print_list(L, R) ++ ")";
pr_str(X, _) -> io_lib:format("error: unknown result~n       ~p", [X]).

print_list(L, R) -> string:join(lists:map(fun (S) -> pr_str(S, R) end, L), " ").

print_str(S) -> lists:reverse(print_str(S, [$"])).
print_str([], Acc) -> [$"|Acc];
print_str([$\\|S], Acc) -> print_str(S, "\\\\" ++ Acc);
print_str([$\n|S], Acc) -> print_str(S, "n\\" ++ Acc);
print_str([$"|S], Acc) -> print_str(S, "\"\\" ++ Acc);
print_str([C|S], Acc) -> print_str(S, [C|Acc]).
