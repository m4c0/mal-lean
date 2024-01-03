-module(reader).
-export([read_str/1]).
%% macros for syntatic sugar
-define(Err(X), {error, X}).
-define(Err2(X), {?Err(X), []}).
-define(ErrC, {?Err(X), _} -> ?Err2(X)).
-define(Sym(X), {symbol, X}).

read_str(Str) ->
  {T, _} = read_form(tokenise(Str)),
  T.

read_form([]) -> ?Err2("empty input");
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
read_form({error, X}) -> ?Err2(X).

read_list(Toks) -> seq(")", list, Toks, []).
read_vector(Toks) -> seq("]", vector, Toks, []).

read_hashmap(Toks) -> mapseq(Toks, #{}).

read_atom({symbol, "nil"}, Toks) -> {nil, Toks};
read_atom({symbol, "true"}, Toks) -> {{boolean, true}, Toks};
read_atom({symbol, "false"}, Toks) -> {{boolean, false}, Toks};
read_atom(T, Toks) -> {T, Toks}.

read_quotish(Sym, Toks) ->
  case read_form(Toks) of
    ?ErrC;
    {T, Rest} -> {{seq, list, [?Sym(Sym), T]}, Rest}
  end.

read_meta(Toks) ->
  case read_form(Toks) of
    {{error, M}, _} -> ?Err2(M);
    {LHS, Rest} ->
      case read_form(Rest) of
        {{error, M}, _} -> ?Err2(M);
        {RHS, FinalRest} -> {{seq, list, [?Sym("with-meta"), RHS, LHS]}, FinalRest}
      end
  end.

seq(_, _, [], _) -> ?Err2("unbalanced sequence");
seq(End, Tp, [End|Toks], Acc) -> {{seq, Tp, lists:reverse(Acc)}, Toks};
seq(End, Tp, Toks, Acc) ->
  case read_form(Toks) of
    ?ErrC;
    {T, Rest} -> seq(End, Tp, Rest, [T|Acc])
  end.

mapseq([], _) -> ?Err2("unbalanced map");
mapseq(["}"|Toks], Acc) -> {{hashmap, Acc}, Toks};
mapseq([K,V|Toks], Acc) when V =/= "}" ->
  case K of
    {KT, _} when KT == string; KT == keyword ->
      case read_form([V|Toks]) of
        ?ErrC;
        {F, Rest} -> mapseq(Rest, Acc#{K => F})
      end;
    _ -> ?Err2("invalid key type")
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
        {error, E} -> ?Err(E);
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
take_str("", _) -> ?Err("EOF while reading string");
take_str([$"|Str], Result) -> {lists:reverse(Result), Str};
take_str([$\\,$\\|Str], Result) -> take_str(Str, [$\\|Result]);
take_str([$\\,$n|Str], Result) -> take_str(Str, [$\n|Result]);
take_str([$\\,$"|Str], Result) -> take_str(Str, [$"|Result]);
take_str([$\\|_], _) -> ?Err("invalid string quoting");
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
