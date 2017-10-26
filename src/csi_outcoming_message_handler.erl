-module(csi_outcoming_message_handler).

-callback init(Opts :: term()) ->
  {ok, State :: term()} | {stop, Reason :: term()}.

-callback allow_outcoming_message(Message :: term(), State :: term()) ->
  {true, NewState :: term()} | {true, NewMessage :: term(), NewState :: term()} |
  {false, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-callback terminate(Reason :: term(), State :: term()) ->
  term().