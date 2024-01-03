-module(env).
-export([new/1,get/2,set/3,find/2]).

new(Outer) -> {map_new(), Outer}.

set({Pid, _}, {symbol, K}, V) -> map_set(Pid, K, V).

find(nil, _) -> error;
find({Pid, Outer}, {symbol, K}) -> 
  case map_find(Pid, K) of
    {ok, _} -> {Pid, Outer};
    error -> find(Outer, {symbol, K})
  end.

get({Pid, Outer}, {symbol, K}) ->
  case find({Pid, Outer}, {symbol, K}) of
    error -> error;
    {OwnerPid, _} -> map_find(OwnerPid, K)
  end.

%% gen_server wrappers

map_new() -> 
  case gen_server:start_link(env_srv, [], []) of
    {ok, Pid} -> Pid;
    _ -> error
  end.

map_set(Pid, K, V) -> gen_server:call(Pid, {set, K, V}).
map_find(Pid, K) -> gen_server:call(Pid, {find, K}).
