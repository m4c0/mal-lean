-module(reader).
-export([read_str/1]).

read_str(Str) ->
  {T, V, _} = read_form(tokenise(Str)),
  {T, V}.

read_form([]) -> {error, "empty input", []};
read_form(["("|Toks]) -> read_list(Toks);
read_form([T|Toks]) -> read_atom(T, Toks);
read_form({error, X}) -> {error, X, []}.

read_list(Toks) -> seq(Toks, []).

read_atom(S, Toks) -> {symbol, S, Toks}.

seq([], _) -> {error, "unbalanced sequence", []};
seq([")"|Toks], Acc) -> {list, lists:reverse(Acc), Toks};
seq(Toks, Acc) ->
  {T, V, Rest} = read_form(Toks),
  case T of
    error -> {T, V, Rest};
    _ -> seq(Rest, [{T, V}|Acc])
  end.

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
        {_, Rest} -> tokenise(Rest, Toks)
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
