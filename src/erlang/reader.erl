-module(reader).
-export([tokenise/2]).

%% read_str() -> read_form(tokenise(), []).

tokenise("", Toks) -> lists:reverse(Toks);

tokenise([C|Str], Toks)
  when C == 32; C == $,
       -> tokenise(Str, Toks);

tokenise("~@" ++ Str, Toks) -> tokenise(Str, ["~@"|Toks]);

tokenise([C|Str], Toks)
  when C == $[; C == $]; C == ${; C == $}; C == $(; C == $);
       C == $'; C == $`; C == $^; C == $~; C == $@
       -> tokenise(Str, [[C]|Toks]).

