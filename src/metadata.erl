-module(metadata).

-include("metadata.hrl").

%% API
-export([
  init/1,
  insert/1,
  delete/1,
  get/1,
  list/0]).

-define(TABLE, ?MODULE).

%%====================================================================
%% API
%%====================================================================

-spec init(Nodes :: list()) -> ok | {error, any()}.
init(Nodes) ->
  case mnesia:create_schema(Nodes) of
    ok ->
      create_table(Nodes);
    {error, {_, {already_exists, _}}} ->
      create_table(Nodes);
    Else ->
      {error, Else}
  end.

create_table(Nodes) ->
  mnesia:start(),

  case mnesia:create_table(?TABLE, [
    {disc_copies, Nodes},
    {attributes, record_info(fields, metadata)}
  ]) of
    {aborted, {already_exists, _}} ->
      ok;
    {aborted, {not_active, ?TABLE, _}} ->
      ok;
    {aborted, _} = X ->
      {error, create_table, X};
    _ ->
      ok
  end.

insert(Metadata = #metadata{}) ->
  F = fun() ->
    mnesia:write(?TABLE, Metadata, write)
      end,
  db_transaction(F).

delete(Name) ->
  F = fun() ->
    mnesia:delete(?TABLE, Name, write)
      end,
  db_transaction(F).

-spec get(Name :: binary()) -> any() | {error, any()}.
get(Name) ->
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
  db_transaction(F).

-spec list() -> {atomic, List :: list()} | {aborted, Reason :: any()}.
list() ->
  F = fun() -> mnesia:foldl(fun(Rec, Acc) ->
    [Rec | Acc]
  end, [], ?TABLE)
      end,
  db_transaction(F).

%%====================================================================
%% Internal functions
%%====================================================================

db_transaction(F) ->
  case mnesia:transaction(F) of
    {atomic, V} ->
      V;
    {aborted, Reason} ->
      {error, Reason}
  end.
