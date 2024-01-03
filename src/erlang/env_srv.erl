-module(env_srv).
-behaviour(gen_server).
-export([init/1,handle_cast/2,handle_call/3,code_change/3]).

init([]) -> {ok, #{}}.

handle_call({find, K}, _, Data) -> {reply, maps:find(K, Data), Data};
handle_call({set, K, V}, _, Data) -> {reply, ok, Data#{K => V}};
handle_call(_, _, State) -> {reply, error, State}.

handle_cast(_, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.

