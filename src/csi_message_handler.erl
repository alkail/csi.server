-module(csi_message_handler).
-behavior(gen_server).

%% API
-export([start_link/1, start_link/2, stop/1, stop/2, command/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_TIMEOUT, 5000).

%%%===================================================================
%%% Internal API
%%%===================================================================

start_link(Opts) ->
  start_link(self(), Opts).

-spec start_link(To :: pid(), Opts) -> {ok, Pid :: pid()} | {error, Reason :: term()}
  when
  Opts :: #{incoming_message_handler=>{IMod :: atom(), IOpts :: term()},
  outcoming_message_handler=>{OMod :: atom(), OOpts :: term()}}.
start_link(To, Opts) ->
  gen_server:start_link(?MODULE, {To, Opts}, []).

stop(Pid) ->
  gen_server:stop(Pid).

stop(Pid, Reason) ->
  gen_server:stop(Pid, Reason, ?DEFAULT_TIMEOUT).

command(Pid, {call, Tag, Command}) ->
  gen_server:cast(Pid, {command, Tag, Command}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({To, Opts}) ->
  {IMod, IModOpts} = maps:get(incoming_message_handler, Opts, {default, []}),
  {OMod, OModOpts} = maps:get(outcoming_message_handler, Opts, {default, []}),
  case {init_mod(IMod, IModOpts), init_mod(OMod, OModOpts)} of
    {{ok, IModState}, {ok, OModState}} ->
      Message = {setup, #{self => self()}},
      send_to_handler(To, Message),
      {ok, #{incoming_mod=>{IMod, IModState}, to=>To, outcoming_mod=>{OMod, OModState}}};
    Err -> Err
  end.

handle_call(_Request, _From, _State) ->
  erlang:error(not_implemented).

handle_cast({command, Tag, Request},
    State = #{to:=To, incoming_mod:=IncomingMod}) ->
  lager:debug("incoming message: ~P", [Request, 9]),
  case handle_incoming_message(Request, IncomingMod) of
    {true, NewModState} ->
      Response = {ok, apply(Request)},
      lager:debug("incoming message response: ~P", [Response, 9]),
      send_to_handler(To, {reply, {Tag, Response}}),
      {noreply, State#{mod_state=>NewModState}};
    {true, NewRequest, NewModState} ->
      Response = {ok, apply(NewRequest)},
      lager:debug("incoming message response: ~P", [Response, 9]),
      send_to_handler(To, {reply, {Tag, Response}}),
      {noreply, State#{mod_state=>NewModState}};
    {false, NewModState} ->
      Response = {error, forbidden},
      lager:debug("incoming message response: ~P", [Response, 9]),
      send_to_handler(To, {reply, {Tag, Response}}),
      {noreply, State#{mod_state=>NewModState}};
    {false, Response, NewModState} ->
      lager:debug("incoming message response: ~P", [Response, 9]),
      send_to_handler(To, {reply, {Tag, Response}}),
      {noreply, State#{mod_state=>NewModState}};
    {stop, Reason, NewModState} ->
      {stop, Reason, State#{mod_state=>NewModState}}
  end.

handle_info(Message, State = #{outcoming_mod:=OutcomingMod, to:=To}) ->
  case handle_outcoming_message(Message, OutcomingMod) of
    {true, NewModState} ->
      {noreply, State#{mod_state=>NewModState}};
    {true, NewMessage, NewModState} ->
      send_to_handler(To, NewMessage),
      {noreply, State#{mod_state=>NewModState}};
    {false, NewModState} ->
      {noreply, State#{mod_state=>NewModState}};
    {stop, Reason, NewModState} ->
      {stop, Reason, State#{mod_state=>NewModState}}
  end.

terminate(Reason, #{incoming_mod:=IMod, outcoming_mod:=OMod}) ->
  terminate_mod(Reason, IMod),
  terminate_mod(Reason, OMod).

code_change(_OldVsn, _State, _Extra) ->
  erlang:error(not_implemented).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_to_handler(To, Message) ->
  catch To ! Message,
  ok.

apply({{M, F}, A}) ->
  erlang:apply(M, F, A);
apply({{M, F}, A, _}) ->
  erlang:apply(M, F, A).

init_mod(default, _) -> {ok, state};
init_mod(Mod, ModOpts) when is_atom(Mod) -> Mod:init(ModOpts).

terminate_mod(_Reason, {default, _ModState}) -> ok;
terminate_mod(Reason, {Mod, ModState}) ->
  catch Mod:terminate(Reason, ModState).

handle_incoming_message(_Message, {default, ModState}) ->
  {true, ModState};
handle_incoming_message(Message, {Mod, ModState}) ->
  Mod:allow_incoming_message(Message, ModState).

handle_outcoming_message(_Message, {default, ModState}) ->
  {true, ModState};
handle_outcoming_message(Message, {Mod, ModState}) ->
  Mod:allow_outcoming_message(Message, ModState).