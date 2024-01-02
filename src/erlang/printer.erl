-module(printer).
-export([pr_str/1]).

pr_str(error) -> "error";
pr_str({symbol, S}) -> S;
pr_str({number, N}) -> N;
pr_str({list, L}) -> "(" ++ print_list(L) ++ ")";
pr_str(X) -> io_lib:format("error: unknown result~n       ~p", [X]).

print_list(L) -> lists:join(" ", lists:map(fun pr_str/1, L)).
