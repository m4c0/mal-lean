-module(reader).
-export([tokenise/2]).

%% read_str() -> read_form(tokenise(), []).

tokenise("", Toks) -> lists:reverse(Toks);

tokenise([C|Str], Toks)
  when C == 32; C == $,
       -> tokenise(Str, Toks);

tokenise("~@" ++ Str, Toks) -> tokenise(Str, ["~@"|Toks]);

tokenise([Chr|Str], Toks) ->
  case Chr of
    C when C == $[; C == $]; C == ${; C == $}; C == $(; C == $);
           C == $'; C == $`; C == $^; C == $~; C == $@
           -> tokenise(Str, [[C]|Toks]);
    $" ->
      case take_str(Str, [$"]) of
        {ValidStr, Rest} -> tokenise(Rest, [ValidStr|Toks])
      end;
    $; ->
      case take_comment(Str, [$;]) of
        {Cmt, Rest} -> tokenise(Rest, [Cmt|Toks])
      end
  end.

take_str("", _) -> error;
take_str([$"|Str], Result) -> {lists:reverse([$"|Result]), Str};
take_str([C|Str], Result) -> take_str(Str, [C|Result]).

take_comment("", Result) -> {lists:reverse(Result), ""};
take_comment([$\n|Str], Result) -> {lists:reverse(Result), Str};
take_comment([C|Str], Result) -> take_comment(Str, [C|Result]).
