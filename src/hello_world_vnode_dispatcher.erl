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
  {ok, MasterNode} = application:get_env(master_node),
  
  case node() of
    MasterNode -> ok;
    _ -> riak_core:join(MasterNode)
  end,
  
  WebPort = case application:get_env(web_port) of
    {ok, P} -> P;
    _ -> 3000
  end,
  
  lager:info("Starting HTTP server on ~p", [WebPort]),
  {ok, Cwd} = file:get_cwd(),
  StaticPath = Cwd ++ "/www",
  lager:info("Serving static resources from ~p", [StaticPath]),
  {ok, ServerPid} = misultin:start_link([
    {port, WebPort}, 
    {backlog, 1000},
    {static, StaticPath},
    {loop, fun dispatch/1},
    {ws_loop, fun ws_dispatch/1}
  ]),
  
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
  misultin:stop(),
  io:format("terminate: ~p ~p~n", [Reason, State]),
  ok.

code_change(OldVsn, State, Extra) ->
  io:format("code_change: ~p ~p ~p~n", [OldVsn, State, Extra]),
  {ok, State}.

dispatch(Req) ->
  Method = Req:get(method), 
  Resource = Req:resource([lowercase, urldecode]),

  Id = mkid(Method, Resource),    
  Hash = riak_core_util:chash_key({Resource, Id}),
  Index = case riak_core_apl:get_primary_apl(Hash, 1, hello_world) of
    [{Idx, _Type}] -> Idx;
    _ -> {0, node()}
  end,
  lager:debug("Dispatching to ~p", [Index]),
  
  case riak_core_vnode_master:sync_spawn_command(Index, {Method, Resource, Req}, hello_world_vnode_master) of
    {ok, R} -> R;
    {error, Reason} -> Req:respond(500, Reason);
    Unhandled -> 
      lager:warning("Unhandled reply: ~p~n", [Unhandled]),
      Req:respond(500)
  end.

ws_dispatch(Ws) ->
  Path = Ws:get(path),
  Headers = Ws:get(headers),
  Id = mkid(websocket, Path),    
  Hash = riak_core_util:chash_key({Path, Id}),
  
  Read = fun(F) ->
    receive
      {browser, Data} ->
        Index = case riak_core_apl:get_primary_apl(Hash, 1, hello_world) of
          [{Idx, _Type}] -> Idx;
          _ -> {0, node()}
        end,
        lager:debug("Dispatching to ~p", [Index]),

        riak_core_vnode_master:command(Index, {websocket, string:tokens(Path, "/"), Headers, Data, Ws}, hello_world_vnode_master),
        F(F);
      _ ->
        F(F)
    end
  end,
  Read(Read).
  
mkid(Method, Resource) ->
  % Absconded from riak_core_util:mkclientid/1
  {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
  {_,_,NowPart} = now(),
  Id = erlang:phash2([Y,Mo,D,H,Mi,S,Method,Resource,NowPart]),
  io_lib:format("~p", [Id]).
