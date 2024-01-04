-module(env).
-export([new/1,new/3,get/2,set/3,find/2]).

new(Outer) -> {map_new(), Outer}.
new(Outer, Binds, Exprs) -> bind(new(Outer), Binds, Exprs).

set({Pid, _}, K, V) -> map_set(Pid, K, V).

find(nil, _) -> error;
find({Pid, Outer}, K) -> 
  case map_find(Pid, K) of
    {ok, _} -> {Pid, Outer};
    error -> find(Outer, K)
  end.

get({Pid, Outer}, K) ->
  case find({Pid, Outer}, K) of
    error -> error;
    {OwnerPid, _} -> map_find(OwnerPid, K)
  end.

%% binder

bind(Env, [], []) -> Env;
bind(Env, [{symbol, "&"},{symbol, B}], Es) ->
  set(Env, B, {seq, list, Es}),
  Env;
bind(_, [{symbol, "&"}|_], _) -> {error, "invalid vararg bind"};
bind(Env, [{symbol, B}|Bs], [E|Es]) ->
  set(Env, B, E),
  bind(Env, Bs, Es);
bind(_, _, _) -> {error, "invalid bind"}.

%% gen_server wrappers

map_new() -> 
  Res = length(get()),
  put({env, Res}, #{}),
  Res.

map_set(Pid, K, V) -> 
  Data = get({env, Pid}),
  put({env, Pid}, Data#{K => V}),
  ok.

map_find(Pid, K) ->
  maps:find(K, get({env, Pid})).
