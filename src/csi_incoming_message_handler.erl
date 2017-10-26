-module(csi_incoming_message_handler).

-callback init(Opts :: term()) ->
  {ok, State :: term()} | {stop, Reason :: term()}.

-callback allow_incoming_message(Message, State :: term()) ->
  {true, NewState :: term()} | {true, NewMessage, NewState :: term()} |
  {false, NewState :: term()} | {false, Response :: term(), NewState :: term()} |
  {stop, Reason :: term(), NewState :: term()}
  when
  Message :: {{Mod :: atom(), Fun :: atom()}, Args :: list(), Info :: term()},
  NewMessage :: {{Mod :: atom(), F :: atom()}, Args :: list(), Info :: term()}
  | {{Mod :: atom(), F :: atom()}, Args :: list()}.

-callback terminate(Reason :: term(), State :: term()) ->
  term().