-module(hello_world_sup).
-behaviour(supervisor).

-include("hello_world_vnode_dispatcher.hrl").

-export([
  start_link/0,
  init/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  Workers = [
    ?GEN_SERVER(hello_world_vnode_dispatcher_master, hello_world_vnode_dispatcher),
    ?VNODE_MASTER(hello_world_vnode_master, hello_world_vnode)
  ],
  
  {ok, {{one_for_one, 10, 10}, Workers}}.