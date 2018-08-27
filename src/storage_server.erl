-module(storage_server).
-behaviour(gen_server).

-include("metadata.hrl").

%% API
-export([
  start_link/0,
  write/3,
  read/1,
  delete/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec write(Name :: binary(), Chunks :: [binary()], Metadata :: #metadata{}) -> ok | {error, Reason :: any()}.
write(Name, Chunks, Metadata = #metadata{}) ->
  gen_server:call(?MODULE, {write, Name, Chunks, Metadata}).

-spec read(Name :: binary()) -> Data :: binary() | {error, Reason :: any()}.
read(Name) ->
  gen_server:call(?MODULE, {read, Name}).

-spec delete(Name :: binary()) -> ok | {error, Reason :: any()}.
delete(Name) ->
  gen_server:call(?MODULE, {delete, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({write, Name, Chunks, Metadata}, _From, State) ->
  Reply = write_chunks(Name, Chunks, Metadata#metadata.chunks),
  {reply, Reply, State};
handle_call({write_internal, Name, ChunkID, Chunk}, _From, State) ->
  Reply = es3_chunk:write({Name, ChunkID}, Chunk),
  {reply, Reply, State};
handle_call({read, Name}, From, State) ->
  case metadata:get(Name) of
    {error, _} = Err ->
      {reply, Err, State};
    #metadata{chunks = MetaChunks} ->
      spawn(fun() ->
        Self = self(),
        request_chunks(Name, Self, MetaChunks),
        Data = wait_for_data(length(MetaChunks), []),
        gen_server:reply(From, Data)
            end),
      {noreply, State}
  end;
handle_call(Request, _From, State) ->
  error_logger:error_msg("Got unexpected call ~p", [Request]),
  {reply, ok, State}.

handle_cast({read_internal, Name, ChunkID, From}, State) ->
  Reply = es3_chunk:read({Name, ChunkID}),
  From ! {chunk, ChunkID, Reply},
  {noreply, State};
handle_cast(Request, State) ->
  error_logger:error_msg("Got unexpected cast ~p", [Request]),
  {noreply, State}.

handle_info(Info, State) ->
  error_logger:error_msg("Got unexpected info ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

write_chunks(_Name, _Chunks, []) ->
  ok;
write_chunks(Name, Chunks, [{Node, ChunkID} | Metadata]) when Node == node() ->
  Chunk = lists:nth(ChunkID, Chunks),
  case es3_chunk:write({Name, ChunkID}, Chunk) of
    ok ->
      write_chunks(Name, Chunks, Metadata);
    {error, Reason} ->
      {error, Reason}
  end;
write_chunks(Name, Chunks, [{Node, ChunkID} | Metadata]) ->
  Chunk = lists:nth(ChunkID, Chunks),
  case gen_server:call({?MODULE, Node}, {write_internal, Name, ChunkID, Chunk}, ?TIMEOUT) of
    ok ->
      write_chunks(Name, Chunks, Metadata);
    {error, Reason} ->
      {error, Reason}
  end.

request_chunks(Name, Self, Metadata) ->
  lists:foldl(fun({Node, ChunkID}, _) ->
    gen_server:cast({?SERVER, Node}, {read_internal, Name, ChunkID, Self})
              end, none, Metadata).

wait_for_data(0, Acc) ->
  join_chunks(lists:keysort(1, Acc), <<>>);
wait_for_data(N, Acc) ->
  receive
    {chunk, ChunkID, Chunk} ->
      wait_for_data(N-1, [{ChunkID, Chunk} | Acc]);
    {error, _} = Err ->
      Err
    after ?TIMEOUT ->
      {error, timeout}
  end.

join_chunks([], Acc) when is_binary(Acc) ->
  Acc;
join_chunks([{_, Chunk} | Chunks], Acc) when is_binary(Acc) ->
  join_chunks(Chunks, <<Acc/binary, Chunk/binary>>).
