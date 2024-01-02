-module(printer).
-export([pr_str/1]).

pr_str(error) -> "error";
pr_str({symbol, S}) -> S;
pr_str({number, N}) -> "Number " ++ N;
pr_str({list, L}) -> "List " ++ L.
