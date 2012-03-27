-module(hello_world_app).
-behaviour(application).

-export([
  start/2, 
  stop/1
]).

start(_StartType, _StartArgs) ->
  case hello_world_sup:start_link() of
    {ok, Pid} ->
      ok = riak_core:register([{vnode_module, hello_world_vnode}]),
      ok = riak_core_node_watcher:service_up(hello_world, self()),
      
      {ok, Pid};
    Else -> Else
  end.

stop(_State) ->
  ok.
