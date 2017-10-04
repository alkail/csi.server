-module(csi_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

init(Req, _Opts) ->
  {ok, Pid} = csi_server:start_link(),
  lager:info("websocket handler initiated", []),
  {cowboy_websocket, Req, Pid}.

websocket_handle({binary, Data}, Req, Pid) ->
  Decoded = erlang:binary_to_term(Data),
  csi_server:command(Pid, Decoded),
  {ok, Req, Pid}.

websocket_info(Info, Req, State) ->
  {reply, {binary, term_to_binary(Info)}, Req, State}.

terminate(Reason, _Req, Pid) ->
  lager:info("websocket handler terminated: Reason=~p", [Reason]),
  ok = csi_server:stop(Pid).
