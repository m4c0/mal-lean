-module(reader).
-export([read_str/1]).

read_str(Str) ->
  {T, V, _} = read_form(tokenise(Str)),
  {T, V}.

read_form([]) -> {error, "empty input", []};
read_form(["("|Toks]) -> read_list(Toks);
read_form(["["|Toks]) -> read_vector(Toks);
read_form(["{"|Toks]) -> read_hashmap(Toks);
read_form(["'"|Toks]) -> read_quotish("quote", Toks);
read_form(["`"|Toks]) -> read_quotish("quasiquote", Toks);
read_form(["~"|Toks]) -> read_quotish("unquote", Toks);
read_form(["~@"|Toks]) -> read_quotish("splice-unquote", Toks);
read_form(["@"|Toks]) -> read_quotish("deref", Toks);
read_form(["^"|Toks]) -> read_meta(Toks);
read_form([T|Toks]) -> read_atom(T, Toks);
read_form({error, X}) -> {error, X, []}.

read_list(Toks) -> seq(")", list, Toks, []).
read_vector(Toks) -> seq("]", vector, Toks, []).

read_hashmap(Toks) -> mapseq(Toks, #{}).

read_atom({symbol, "nil"}, Toks) -> {nil, nil, Toks};
read_atom({symbol, "true"}, Toks) -> {boolean, true, Toks};
read_atom({symbol, "false"}, Toks) -> {boolean, false, Toks};
read_atom({T, V}, Toks) -> {T, V, Toks};
read_atom(_, _) -> {error, "invalid input", []}.

read_quotish(Sym, Toks) ->
  case read_form(Toks) of
    {error, M, _} -> {error, M, []};
    {T, V, Rest} -> {list, [{symbol, Sym}, {T, V}], Rest}
  end.

read_meta(Toks) ->
  case read_form(Toks) of
    {error, M, _} -> {error, M, []};
    {LT, LV, Rest} ->
      case read_form(Rest) of
        {error, M, _} -> {error, M, []};
        {RT, RV, FinalRest} ->
          {list, [{symbol, "with-meta"}, {RT, RV}, {LT, LV}], FinalRest}
      end
  end.

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

    C when C >= $0, C =< $9 ->
      case take_number(Str, C - $0) of
        {Number, Rest} -> tokenise(Rest, [{number, Number}|Toks])
      end;

    C when C == $- ->
      case Str of
        [CC|SS] when CC >= $0, CC =< $9 ->
          {Number, Rest} = take_number(SS, CC - $0),
          tokenise(Rest, [{number, -Number}|Toks]);
        _ ->
          case take_symbol(Str, [Chr]) of
            {Sym, Rest} -> tokenise(Rest, [{symbol, Sym}|Toks])
          end
      end;

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

take_number([C|Str], Acc) when C >= $0, C =< $9 -> 
  take_number(Str, Acc * 10 + (C - $0));
take_number(X, Acc) -> {Acc, X}.

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
