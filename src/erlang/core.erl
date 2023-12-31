-module(core).
-export([ns/0]).

ns() ->
  maps:map(fun (_, V) -> {lambda, V} end,
           #{"+" => fun add/1,
             "-" => fun sub/1,
             "*" => fun mult/1,
             "/" => fun dv/1,
             "<" => fun lt/1,
             ">" => fun gt/1,
             "<=" => fun lte/1,
             ">=" => fun gte/1,
             "=" => fun eq/1,
             "apply" => fun applyi/1,
             "assoc" => fun assoc/1,
             "atom" => fun atom/1,
             "atom?" => fun atomq/1,
             "concat" => fun concat/1,
             "conj" => fun conj/1,
             "cons" => fun cons/1,
             "contains?" => fun contains/1,
             "count" => fun count/1,
             "deref" => fun deref/1,
             "dissoc" => fun dissoc/1,
             "empty?" => fun empty/1,
             "false?" => fun falseq/1,
             "fn?" => fun fnq/1,
             "first" => fun first/1,
             "get" => fun geti/1,
             "hash-map" => fun hashmap/1,
             "keys" => fun keys/1,
             "keyword" => fun keyword/1,
             "keyword?" => fun keywordq/1,
             "list" => fun list/1,
             "list?" => fun listq/1,
             "macro?" => fun macroq/1,
             "map" => fun mapi/1,
             "map?" => fun mapq/1,
             "nil?" => fun nilq/1,
             "number?" => fun numberq/1,
             "nth" => fun nth/1,
             "println" => fun println/1,
             "prn" => fun prn/1,
             "pr-str" => fun prstr/1,
             "reset!" => fun reset/1,
             "rest" => fun rest/1,
             "read-string" => fun readstring/1,
             "seq" => fun seq/1,
             "sequential?" => fun sequential/1,
             "slurp" => fun slurp/1,
             "str" => fun str/1,
             "string?" => fun stringq/1,
             "symbol" => fun symbol/1,
             "symbol?" => fun symbolq/1,
             "swap!" => fun swap/1,
             "time-ms" => fun timems/1,
             "true?" => fun trueq/1,
             "throw" => fun throwi/1,
             "vals" => fun vals/1,
             "vec" => fun vec/1,
             "vector" => fun vector/1,
             "vector?" => fun vectorq/1}).

add([{number, A},{number, B}]) -> {number, A+B};
add(_) -> {error, "invalid parameters"}.

sub([{number, A},{number, B}]) -> {number, A-B};
sub(_) -> {error, "invalid parameters"}.

mult([{number, A},{number, B}]) -> {number, A*B};
mult(_) -> {error, "invalid parameters"}.

dv([{number, A},{number, B}]) -> {number, A div B};
dv(_) -> {error, "invalid parameters"}.

lt([{number, A},{number, B}]) -> {boolean, A < B};
lt(_) -> {error, "invalid parameters"}.

gt([{number, A},{number, B}]) -> {boolean, A > B};
gt(_) -> {error, "invalid parameters"}.

lte([{number, A},{number, B}]) -> {boolean, A =< B};
lte(_) -> {error, "invalid parameters"}.

gte([{number, A},{number, B}]) -> {boolean, A >= B};
gte(_) -> {error, "invalid parameters"}.

eq([{seq, _, A},{seq, _, B}]) -> seq_eq(A, B);
eq([{hashmap, A},{hashmap, B}]) -> map_eq(A, B);
eq([A,B]) -> {boolean, A == B};
eq(_) -> {error, "invalid parameters for ="}.

applyi([{lambda, Fn},E|L]) -> Fn(flat([E|L]));
applyi(_) -> {error, "invalid parameters for apply"}.

assoc([{hashmap, _}=E]) -> E;
assoc([{hashmap, M},{T, _}=K,V|As]) when T == string; T == keyword ->
  assoc([{hashmap, M#{K => V}}|As]);
assoc(_) -> {error, "invalid parameters for assoc"}.

atom([X]) ->
  Id = {atom, length(get())},
  put(Id, X),
  Id;
atom(_) -> {error, "invalid parameters for atom"}.

atomq(X) -> check_type(atom, X).

concat(X) -> concat(X, []).
concat([], Acc) -> {seq, list, Acc};
concat([{seq, _, L}|Rest], Acc) -> concat(Rest, Acc ++ L);
concat(_, _) -> {error, "invalid parameters for concat"}.

conj([{seq, list, L}|Rest]) -> {seq, list, conj_list(Rest, L)};
conj([{seq, vector, L}|Rest]) -> {seq, vector, conj_vec(Rest, L)};
conj(_) -> {error, "invalid parameters for conj"}.

cons([V, {seq, _, L}]) -> {seq, list, [V|L]};
cons(_) -> {error, "invalid parameters for cons"}.

contains([{hashmap, M}, {T, _}=K]) when T == string; T == keyword ->
  case maps:find(K, M) of
    error -> {boolean, false};
    _ -> {boolean, true}
  end;
contains(_) -> {error, "invalid parameters for contains?"}.

count([{seq, _, L}]) -> {number, length(L)};
count([nil]) -> {number, 0};
count(_) -> {error, "invalid parameters for count"}.

deref([{atom, _} = X]) -> get(X);
deref(_) -> {error, "invalid parameters for deref"}.

dissoc([{hashmap, _} = X]) -> X;
dissoc([{hashmap, M},{T, _}=K|As]) when T == string; T == keyword ->
  dissoc([{hashmap, maps:remove(K, M)}|As]);
dissoc(_) -> {error, "invalid parameters for dissoc"}.

empty([{seq, _, []}]) -> {boolean, true};
empty([_]) -> {boolean, false};
empty(_) -> {error, "invalid parameters for empty?"}.

falseq([{boolean, false}]) -> {boolean, true};
falseq([_]) -> {boolean, false};
falseq(_) -> {error, "invalid parameters for false?"}.

first([nil]) -> nil;
first([{seq, _, []}]) -> nil;
first([{seq, _, [E|_]}]) -> E;
first(_) -> {error, "invalid parameters for first"}.

fnq(X) -> check_type(lambda, X).

geti([nil, _]) -> nil;
geti([{hashmap, M},{T, _}=K]) when T == string; T == keyword ->
  case maps:find(K, M) of
    error -> nil;
    {ok, V} -> V
  end;
geti(_) -> {error, "invalid parameters for get"}.

hashmap(L) -> hashmap(L, #{}).
hashmap([], Acc) -> {hashmap, Acc};
hashmap([{T, _}=K,V|L], Acc) when T == string; T == keyword ->
  hashmap(L, Acc#{K => V});
hashmap(_, _) -> {error, "invalid parameters for hash-map"}.

keys([{hashmap, M}]) -> {seq, list, maps:keys(M)};
keys(_) -> {error, "invalid parameters for keys"}.

keyword([{string, S}]) -> {keyword, ":" ++ S};
keyword([{keyword, _} = X]) -> X;
keyword(_) -> {error, "invalid parameters for keyword"}.

keywordq(X) -> check_type(keyword, X).

list(X) -> {seq, list, X}.

listq([{seq, list, _}]) -> {boolean, true};
listq([_]) -> {boolean, false};
listq(_) -> {error, "invalid parameters for list?"}.

macroq(X) -> check_type(macro, X).

mapi([{lambda, Fn},{seq, _, L}]) -> mapi(Fn, L, []);
mapi(_) -> {error, "invalid parameters for map"}.
mapi(_, [], Acc) -> {seq, list, lists:reverse(Acc)};
mapi(Fn, [E|L], Acc) ->
  case Fn([E]) of
    {error, _} = X -> X;
    X -> mapi(Fn, L, [X|Acc])
  end.

mapq(X) -> check_type(hashmap, X).

nilq([nil]) -> {boolean, true};
nilq([_]) -> {boolean, false};
nilq(_) -> {error, "invalid parameters for nil?"}.

nth([{seq, _, L},{number, N}]) when N >= 0 -> nth(L, N);
nth(_) -> {error, "invalid parameters for nth"}.
nth([E|_], 0) -> E;
nth([_|L], N) when N > 0 -> nth(L, N - 1);
nth(_, _) -> {error, "invalid range for nth"}.

numberq(X) -> check_type(number, X).

println(L) -> iofmt(L, "", false).

prn(L) -> iofmt(L, "", true).

prstr(L) -> fmt(L, " ", true).

reset([{atom, _} = X, V]) -> put(X, V), V;
reset(_) -> {error, "invalid parameters for reset"}.

rest([nil]) -> {seq, list, []};
rest([{seq, _, []}]) -> {seq, list, []};
rest([{seq, _, [_|L]}]) -> {seq, list, L};
rest(_) -> {error, "invalid paramters for rest"}.

readstring([{string, X}]) -> reader:read_str(X);
readstring(_) -> {error, "invalid parameters for read-string"}.

seq([{string, [_|_]=S}]) ->
  L = lists:map(fun (C) -> {string, [C]} end, S),
  {seq, list, L};
seq([{seq, _, [_|_]=L}]) -> {seq, list, L};
seq(_) -> nil.

sequential([{seq, _, _}]) -> {boolean, true};
sequential([_]) -> {boolean, false};
sequential(_) -> {error, "invalid parameters for sequential?"}.

slurp([{string, X}]) ->
  case file:read_file(X) of
    {ok, Bin} -> {string, binary_to_list(Bin)};
    _ -> {error, "file not found"}
  end;
slurp(_) -> {error, "invalid parameters for slurp"}.

str(L) -> fmt(L, "", false).

stringq(X) -> check_type(string, X).

swap([{atom, _} = X,{lambda, Fn}|As]) ->
  V = Fn([get(X)|As]),
  put(X, V),
  V;
swap(_) -> {error, "invalid parameters for swap"}.

symbol([{string, S}]) -> {symbol, S};
symbol(_) -> {error, "invalid parameters for symbol"}.

symbolq(X) -> check_type(symbol, X).

throwi([X]) -> {error, X};
throwi(_) -> {error, "invalid parameters for throw"}.

timems([]) -> {number, erlang:system_time(millisecond)};
timems(_) -> {error, "invalid parameters for time-ms"}.

trueq([{boolean, true}]) -> {boolean, true};
trueq([_]) -> {boolean, false};
trueq(_) -> {error, "invalid parameters for true?"}.

vals([{hashmap, M}]) -> {seq, list, maps:values(M)};
vals(_) -> {error, "invalid parameters for vals"}.

vec([{seq, _, L}]) -> {seq, vector, L};
vec(_) -> {error, "invalid parameters for vec"}.

vector(L) -> {seq, vector, L}.

vectorq([{seq, vector, _}]) -> {boolean, true};
vectorq([_]) -> {boolean, false};
vectorq(_) -> {error, "invalid parameters for vector?"}.

%% helpers

fmt(L, Sep, Fmt) ->
  NL = lists:map(fun (V) -> printer:pr_str(V, Fmt) end, L),
  {string, string:join(NL, Sep)}.

iofmt([], _, _) -> io:format("~n", []), nil;
iofmt([X|Xs], Sep, Fmt) -> 
  io:format("~s~s", [Sep, printer:pr_str(X, Fmt)]),
  iofmt(Xs, " ", Fmt).

seq_eq([], []) -> {boolean, true};
seq_eq([A|AA],[B|BB]) ->
  case eq([A, B]) of
    {boolean, true} -> seq_eq(AA, BB);
    X -> X
  end;
seq_eq(_, _) -> {boolean, false}.

map_eq(A, B) ->
  M = maps:merge_with(fun (_, VA, VB) -> {pair, VA, VB} end, A, B),
  maps:fold(fun (_, _, {boolean, false}) -> false;
                (_, {pair, PA, PB}, _) -> eq([PA, PB]);
                (_, _, _) -> {boolean, false} end,
            {boolean, true}, M).

%% flat in fold-right style - inefficient, but works
flat(L) -> flat(L, []).
flat([], Acc) -> Acc;
flat([{seq, _, S}|L], Acc) -> flat(L, Acc ++ S);
flat([X|L], Acc) -> flat(L, Acc ++ [X]).

check_type(X, [{X, _}]) -> {boolean, true};
check_type(_, [_]) -> {boolean, false};
check_type(_, _) -> {error, "invalid parameters"}.

conj_list([], Acc) -> Acc;
conj_list([E|L], Acc) -> conj_list(L, [E|Acc]).

conj_vec(Rest, List) -> List ++ Rest.
