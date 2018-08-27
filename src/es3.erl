-module(es3).

-include("metadata.hrl").

-export([write/2, read/1, delete/1]).

-spec write(Name, Object) -> Res when
  Name   :: iodata(),
  Object :: binary(),
  Res    :: ok | {error, Reason :: any()}.
write(Name, Object) ->
  ChunkSize = application:get_env(es3, chunk_size, 32768),
  Chunks = split_chunks(Object, ChunkSize),

  ChunkMetadata = build_chunk_metadata(Chunks),

  Metadata = #metadata{
    id = Name,
    chunks = ChunkMetadata
  },

  metadata:insert(Metadata),
  storage_server:write(Name, Chunks, Metadata).

-spec read(Name) -> Object when
  Name   :: iodata(),
  Object :: binary() | {error, Reason :: any()}.
read(Name) ->
  case storage_server:read(Name) of
    Data when is_binary(Data) ->
      Data;
    {error, _} = Err ->
      Err
  end.

-spec delete(Name) -> Res when
  Name :: iodata(),
  Res  :: ok | {error, Reason :: any()}.
delete(Name) ->
  storage_server:delete(Name).

%%====================================================================
%% Internal functions
%%====================================================================
split_chunks(Object, ChunkSize) ->
  lists:reverse(split_chunks(Object, ChunkSize, [])).

split_chunks(Object, ChunkSize, Acc) when byte_size(Object) >= ChunkSize ->
  {Chunk, Rest} = split_binary(Object, ChunkSize),
  split_chunks(Rest, ChunkSize, [binary:copy(Chunk)|Acc]);
split_chunks(<<>>, _ChunkSize, Acc) ->
  Acc;
split_chunks(Object, _ChunkSize, Acc) ->
  [binary:copy(Object)|Acc].

build_chunk_metadata(Parts) ->
  Nodes = application:get_env(es3, nodes, [node()|nodes()]),

  PartIDs = lists:seq(1, length(Parts)),

  AllNodes = set_length(Nodes, length(PartIDs)),

  lists:zip(AllNodes, PartIDs).

%% Fix the length of L to N elements long
set_length(L, N) when length(L) < N ->
  lists:reverse(set_length(L, N-length(L), lists:reverse(L)));
set_length(L, N) when length(L) > N ->
  lists:sublist(L, N);
set_length(L, _N) ->
  L.

set_length(_L, 0, Acc) ->
  Acc;
set_length([H|T], N, Acc) ->
  set_length(T ++ [H], N-1, [H|Acc]).

%%====================================================================
%% EUnit tests
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

split_chunks_test() ->
  ?assert(split_chunks(<<"test">>, 32768) == [<<"test">>]).

split_chunks_2_test() ->
  ?assert(split_chunks(<<"test">>, 1) == [<<"t">>, <<"e">>, <<"s">>, <<"t">>]).

fix_length_test() ->
  % Equal length
  ?assert(set_length([1], 1) == [1]),

  % List too long
  ?assert(set_length([1,2,3], 1) == [1]),
  ?assert(set_length([1,2,3], 2) == [1,2]),

  % List too short
  ?assert(set_length([1,2,3], 6) == [1,2,3,1,2,3]),
  ?assert(set_length([1,2,3], 5) == [1,2,3,1,2]).

-endif.