-module(csi_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

init(Req, Opts) ->
  {ok, Pid} = csi_message_handler:start_link(Opts),
  lager:info("websocket handler initiated"),
  {cowboy_websocket, Req, Pid}.

websocket_handle({binary, Data}, Req, Pid) ->
  Decoded = erlang:binary_to_term(Data),
  csi_message_handler:command(Pid, Decoded),
  {ok, Req, Pid}.

websocket_info(Info, Req, Pid) ->
  {reply, {binary, term_to_binary(Info)}, Req, Pid}.

terminate(Reason, _Req, Pid) ->
  lager:info("websocket handler terminated: Reason=~p", [Reason]),
  csi_message_handler:stop(Pid).