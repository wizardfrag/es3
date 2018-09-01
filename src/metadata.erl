-module(metadata).

-behaviour(gen_server).

-include("metadata.hrl").

%% API
-export([
  start_link/0,
  start_link/1,
  insert/1,
  delete/1,
  get/1,
  list/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, metadata).

-record(state, {status, nodes}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [node() | nodes()], []).

start_link(Nodes) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Nodes, []).

-spec insert(Metadata :: #metadata{}) -> ok | {error, Reason :: any()}.
insert(Metadata = #metadata{}) ->
  gen_server:call(?SERVER, {insert, Metadata}).

-spec delete(Name :: string()) -> ok | {error, Reason :: any()}.
delete(Name) ->
  gen_server:call(?SERVER, {delete, Name}).

-spec get(Name :: binary()) -> #metadata{} | {error, any()}.
get(Name) ->
  gen_server:call(?SERVER, {get, Name}).

-spec list() -> List :: list() | {aborted, Reason :: any()}.
list() ->
  gen_server:call(?SERVER, list).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Nodes) ->
  self() ! start_mnesia,
  error_logger:info_msg("Starting metadata server ~p~n", [self()]),
  {ok, #state{status = not_started, nodes = Nodes}}.

handle_call({insert, Metadata}, _From, State = #state{status = ok}) ->
  F = fun() ->
    mnesia:write(?TABLE, Metadata, write)
      end,
  Reply = db_transaction(F),
  {reply, Reply, State};
handle_call({get, Name}, _From, State = #state{status = ok}) ->
  F = fun() ->
    case mnesia:read(?TABLE, Name, read) of
      [V] ->
        V;
      [] ->
        {error, not_found};
      X ->
        {error, X}
    end
      end,
  Reply = db_transaction(F),
  error_logger:info_msg("Reply = ~p~n", [Reply]),
  {reply, Reply, State};
handle_call({delete, Name}, _From, State = #state{status = ok}) ->
  F = fun() ->
    mnesia:delete(?TABLE, Name, write)
      end,
  Reply = db_transaction(F),
  {reply, Reply, State};
handle_call(list, _From, State = #state{status = ok}) ->
  F = fun() -> mnesia:foldl(fun(Rec, Acc) ->
    [Rec | Acc]
                            end, [], ?TABLE)
      end,
  Reply = db_transaction(F),
  {reply, Reply, State};
handle_call(Request, _From, State = #state{status = not_started}) ->
  error_logger:warning_msg("Received call ~p in not_started state~n", [Request]),
  {reply, {error, not_started}, State};
handle_call(Request, _From, State) ->
  error_logger:warning_msg("Received unknown call ~p~n", [Request]),
  {reply, {error, unknown}, State}.

handle_cast(Request, State) ->
  error_logger:warning_msg("Received unknown cast ~p~n", [Request]),
  {noreply, State}.

handle_info(start_mnesia, State = #state{status = not_started, nodes = Nodes}) ->
  case start_mnesia(Nodes) of
    ok ->
      error_logger:info_msg("Started mnesia~n"),
      {noreply, State#state{status = ok}};
    X ->
      error_logger:warning_msg("Could not start mnesia: ~p - retrying~n", [X]),
      erlang:send_after(1000, ?SERVER, start_mnesia),
      {noreply, State}
  end;
handle_info(Info, State) ->
  error_logger:warning_msg("Received unknown info ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_mnesia(Nodes) ->
  case mnesia:create_schema(Nodes) of
    ok ->
      create_table(Nodes);
    {error, {_Node, {already_exists, _ProblemNode}}} ->
      error_logger:warning_msg("mnesia:create_schema returned already_exists~n"),
      create_table(Nodes);
    X ->
      {error, create_schema, X}
  end.

create_table(Nodes) ->
  mnesia:start(),

  case mnesia:create_table(?TABLE, [
    {disc_copies, Nodes},
    {attributes, record_info(fields, metadata)}
  ]) of
    {aborted, {already_exists, _}} ->
      error_logger:warning_msg("mnesia:create_table returned already_exists~n"),
      ok;
    {aborted, _} = X ->
      {error, {create_table, X}};
    _ ->
      ok
  end.

db_transaction(F) ->
  case mnesia:transaction(F) of
    {atomic, V} ->
      V;
    {aborted, Reason} ->
      {error, Reason}
  end.
