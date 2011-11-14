-module(hello_world_vnode_dispatcher).
-behaviour(gen_server2).

-include("hello_world_vnode_dispatcher.hrl").
-include_lib("misultin/include/misultin.hrl").

-export([
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, { site, server, config }).

start_link() ->
  gen_server2:start_link({local, ?MODULE}, ?MODULE, [], [{timeout, infinity}]).

init([]) ->
  process_flag(trap_exit, true),

  {ok, ServerPid} = misultin:start_link([{port, 3000}, {loop, fun(Req) -> 
    
    Method = Req:get(method), 
    Resource = Req:resource([lowercase, urldecode]),

    Hash = riak_core_util:chash_key({Method, Resource}),
    case riak_core_apl:get_primary_apl(Hash, 1, hello_world) of
      [{Index, _Type}] ->
          case riak_core_vnode_master:sync_spawn_command(Index, {Method, Resource, Req}, hello_world_vnode_master) of
            {ok, Resp} -> 
              io:format("response: ~p~n", [Resp]),
              Resp;
            {error, Reason} -> Req:respond(500, Reason)
          end;
       {error, Reason} -> Req:respond(500, Reason)
    end
    
  end}]),
  
  
  {ok, #state { server = ServerPid }}.

handle_call(Msg, From, State) ->
  io:format("handle_call: ~p ~p ~p~n", [Msg, From, State]),
  {noreply, State}.

handle_cast(Msg, State) ->
  io:format("handle_cast: ~p ~p~n", [Msg, State]),
  {noreply, State}.

handle_info(Msg, State) ->
  io:format("handle_info: ~p ~p~n", [Msg, State]),
  {noreply, State}.

terminate(Reason, State) ->
  io:format("terminate: ~p ~p~n", [Reason, State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  % io:format("code_change: ~p ~p ~p~n", [OldVsn, State, Extra]),
  {ok, State}.
