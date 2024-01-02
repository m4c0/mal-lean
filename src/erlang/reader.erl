-module(reader).
-export([read_str/1]).

read_str(Str) ->
  {T, V, _} = read_form(tokenise(Str)),
  {T, V}.

read_form([]) -> {error, "empty input", []};
read_form(["("|Toks]) -> read_list(Toks);
read_form(["["|Toks]) -> read_vector(Toks);
read_form(["{"|Toks]) -> read_hashmap(Toks);
read_form([T|Toks]) -> read_atom(T, Toks);
read_form({error, X}) -> {error, X, []}.

read_list(Toks) -> seq(")", list, Toks, []).
read_vector(Toks) -> seq("]", vector, Toks, []).

read_hashmap(Toks) -> mapseq(Toks, #{}).

read_atom({T, V}, Toks) -> {T, V, Toks};
read_atom(_, _) -> {error, "invalid input", []}.

seq(_, _, [], _) -> {error, "unbalanced sequence", []};
seq(End, Tp, [End|Toks], Acc) -> {Tp, lists:reverse(Acc), Toks};
seq(End, Tp, Toks, Acc) ->
  {T, V, Rest} = read_form(Toks),
  case T of
    error -> {T, V, Rest};
    _ -> seq(End, Tp, Rest, [{T, V}|Acc])
  end.

mapseq([], _) -> {error, "unbalanced map", []};
mapseq(["}"|Toks], Acc) -> {hashmap, Acc, Toks};
mapseq([K,V|Toks], Acc) when V =/= "}" ->
  case K of
    {KT, _} when KT == string; KT == keyword ->
      case read_form([V|Toks]) of
        {error, M, _} -> {error, M, []};
        {FT, FV, Rest} -> mapseq(Rest, Acc#{K => {FT, FV}})
      end;
    _ -> {error, "invalid key type", []}
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
      case take_str(Str) of
        {error, E} -> {error, E};
        {ValidStr, Rest} -> tokenise(Rest, [{string, ValidStr}|Toks])
      end;
    $; ->
      case take_comment(Str, [$;]) of
        {_, Rest} -> tokenise(Rest, Toks)
      end;
    $: ->
      case take_symbol(Str, [Chr]) of
        {Sym, Rest} -> tokenise(Rest, [{keyword, Sym}|Toks])
      end;
    _ ->
      case take_symbol(Str, [Chr]) of
        {Sym, Rest} -> tokenise(Rest, [{symbol, Sym}|Toks])
      end
  end.

take_str(Str) -> take_str(Str, []).
take_str("", _) -> {error, "EOF while reading string"};
take_str([$"|Str], Result) -> {lists:reverse(Result), Str};
take_str([$\\,$\\|Str], Result) -> take_str(Str, [$\\|Result]);
take_str([$\\,$n|Str], Result) -> take_str(Str, [$\n|Result]);
take_str([$\\,$"|Str], Result) -> take_str(Str, [$"|Result]);
take_str([$\\|_], _) -> {error, "invalid string quoting"};
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
