-module(macro).
-export([expand/2]).

expand(X, _) -> X.
