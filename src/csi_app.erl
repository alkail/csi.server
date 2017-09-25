-module(csi_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
		{'_', [
			{"/ws", csi_handler, []}
		]}
	]),
  Port = application:get_env(csi, port, 8080),
	{ok, _} = cowboy:start_http(http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
  lager:info("CSI websocket listener started on port: ~p", [Port]),
  {ok, self()}.

stop(_State) ->
  ok.
