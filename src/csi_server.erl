-module(csi_server).

-behaviour(gen_server).

-export([start_link/0, stop/1, command/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, self(), []).

stop(Pid) ->
    gen_server:stop(Pid).

command(Pid, Command) ->
    gen_server:cast(Pid, {command, Command}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Handler) ->
    Message = {setup, #{self => self()}},
    send_to_handler(Handler, Message),
    {ok, Handler}.

handle_call({command, {send, Pid, Message}}, _From, State) ->
    Pid ! Message,
    {reply, ok, State}.

handle_cast({command, {call, Correlation, {Module, Function}, Params}}, Handler) ->
    lager:debug("incoming call request: ~p:~p(~P)", [Module, Function, Params, 9]),
    Result = erlang:apply(Module, Function, Params),
    lager:debug("outcoming call response: (~P)",[Result, 9]),
    Message = {reply, {Correlation, Result}},
    send_to_handler(Handler, Message),
    {noreply, Handler};

handle_cast(_Msg, Handler) ->
    {noreply, Handler}.

handle_info(Message, Handler) ->
    send_to_handler(Handler, {message, Message}),
    {noreply, Handler}.

terminate(_Reason, _Handler) ->
    ok.

code_change(_OldVsn, Handler, _Extra) ->
    {ok, Handler}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_to_handler(Handler, Message)->
    Handler ! Message.