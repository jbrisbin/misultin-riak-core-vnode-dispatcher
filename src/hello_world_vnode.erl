-module(hello_world_vnode).
-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").
-include_lib("misultin/include/misultin.hrl").

-export([
  delete/1,
  encode_handoff_item/2,
  handle_command/3,
  handle_coverage/4,
  handle_exit/3,
  handle_handoff_command/3,
  handle_handoff_data/2,
  handoff_cancelled/1,
  handoff_finished/2,
  handoff_starting/2,
  init/1,
  is_empty/1,
  start_vnode/1,
  terminate/2
]).

-record(state, {
  partition :: partition()
}).

start_vnode(I) ->
  io:format("starting ~p~n", [I]),
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
  io:format("started hello world vnode at ~p~n", [Partition]),
  {ok, #state { partition = Partition }}.

handle_command({'GET', [], Req}, _Sender, State) ->
  io:format("from misultin: ~p~n", [Req]),
  Response = Req:ok([{"Content-Type", "text/plain"}], <<"Hello World!">>),
  {reply, {ok, Response}, State};

handle_command(Msg, _Sender, State) ->
  io:format("unhandled command: ~p~n", [Msg]),
  {noreply, State}.

handle_coverage(Request, KeySpaces, Sender, State) ->
  io:format("handle_coverage: ~p ~p ~p ~p~n", [Request, KeySpaces, Sender, State]),
  {continue, State}.
  
handle_exit(_Pid, Reason, State) ->
  {stop, Reason, State}.

handle_handoff_command(_Message, _Sender, State) ->
  {forward, State}.

handoff_starting(_TargetNode, State) ->
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_TargetNode, State) ->
  {ok, State}.

handle_handoff_data(_Data, State) ->
  {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
  <<>>.

is_empty(State) ->
  {true, State}.

delete(State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
