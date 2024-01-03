-module(env).
-export([new/1,get/2,set/3,find/2]).

new(Outer) -> {map_new(), Outer}.

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

%% gen_server wrappers

map_new() -> 
  case gen_server:start_link(env_srv, [], []) of
    {ok, Pid} -> Pid;
    _ -> error
  end.

map_set(Pid, K, V) -> gen_server:call(Pid, {set, K, V}).
map_find(Pid, K) -> gen_server:call(Pid, {find, K}).
