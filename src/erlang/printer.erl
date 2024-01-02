-module(printer).
-export([pr_str/1]).

pr_str({error, E}) -> "error: " ++ E;
pr_str({symbol, S}) -> S;
pr_str({number, N}) -> N;
pr_str({list, L}) -> "(" ++ print_list(L) ++ ")";
pr_str(X) -> io_lib:format("error: unknown result~n       ~p", [X]).

print_list(L) -> string:join(lists:map(fun pr_str/1, L), " ").
