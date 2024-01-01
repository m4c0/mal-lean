-module(printer).
-export([pr_str/1]).

pr_str(X) -> ["C"|X].
