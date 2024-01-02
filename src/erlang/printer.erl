-module(printer).
-export([pr_str/2]).

pr_str({error, E}, _) -> "error: " ++ E;
pr_str({symbol, S}, _) -> S;
pr_str({number, N}, _) -> N;
pr_str({string, S}, _) -> S;
pr_str({list, L}, R) -> "(" ++ print_list(L, R) ++ ")";
pr_str(X, _) -> io_lib:format("error: unknown result~n       ~p", [X]).

print_list(L, R) -> string:join(lists:map(fun (S) -> pr_str(S, R) end, L), " ").
