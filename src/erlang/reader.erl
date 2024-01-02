-module(reader).
-export([read_str/1]).

read_str(Str) -> read_form(tokenise(Str)).

read_form([T]) -> read_atom(T);
read_form(["("|Toks]) -> read_list(Toks);
read_form({error, X}) -> {error, X};
read_form(_) -> error.

read_list(Toks) -> seq(Toks, []).

read_atom(S) -> {symbol, S}.

seq([], _) -> error;
seq([")"], Acc) -> {list, lists:reverse(Acc)};
seq([S|Toks], Acc) -> seq(Toks, [read_atom(S)|Acc]).

%% Tokeniser

tokenise(Str) -> tokenise(Str, []).

tokenise("", Toks) -> lists:reverse(Toks);
tokenise("~@" ++ Str, Toks) -> tokenise(Str, ["~@"|Toks]);
tokenise([Chr|Str], Toks) ->
  case Chr of
    C when C == $\s; C == $,; C == $\n; C == $\r; C == $\t
           -> tokenise(Str, Toks);

    C when C == $[; C == $]; C == ${; C == $}; C == $(; C == $);
           C == $'; C == $`; C == $^; C == $~; C == $@
           -> tokenise(Str, [[C]|Toks]);
    $" ->
      case take_str(Str, [$"]) of
        {error, E} -> {error, E};
        {ValidStr, Rest} -> tokenise(Rest, [ValidStr|Toks])
      end;
    $; ->
      case take_comment(Str, [$;]) of
        {Cmt, Rest} -> tokenise(Rest, [Cmt|Toks])
      end;
    _ ->
      case take_symbol(Str, [Chr]) of
        {Sym, Rest} -> tokenise(Rest, [Sym|Toks])
      end
  end.

take_str("", _) -> {error, "EOF while reading string"};
take_str([$"|Str], Result) -> {lists:reverse([$"|Result]), Str};
take_str([$\\,C|Str], Result) -> take_str(Str, [C|Result]);
take_str([C|Str], Result) -> take_str(Str, [C|Result]).

take_comment("", Result) -> {lists:reverse(Result), ""};
take_comment([$\n|Str], Result) -> {lists:reverse(Result), Str};
take_comment([C|Str], Result) -> take_comment(Str, [C|Result]).

take_symbol("", Result) -> {lists:reverse(Result), ""};
take_symbol([Chr|Str], Result) ->
  case Chr of
    C when C == $\s; C == $,; C == $\n; C == $\r; C == $\t;
           C == $[; C == $]; C == ${; C == $}; C == $(; C == $);
           C == $'; C == $`; C == $^; C == $~; C == $@;
           C == $"; C == $;
           -> {lists:reverse(Result), [Chr|Str]};
    _ -> take_symbol(Str, [Chr|Result])
  end.
