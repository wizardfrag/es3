-module(es3_chunk).

-export([write/2, read/1, delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec write(Key :: any(), Chunk :: binary()) -> ok | {error, Reason :: any()}.
write({Name, ID}, Chunk) ->
  FullPath = dirname(Name),
  case file:make_dir(FullPath) of
    ok ->
      write_file(FullPath, ID, Chunk);
    {error, eexist} ->
      write_file(FullPath, ID, Chunk);
    Err ->
      {error, Err}
  end.

-spec read(Key :: any()) -> Chunk :: binary() | {error, Reason :: any()}.
read({Name, ID}) ->
  FullPath = dirname(Name),
  FullName = filename:join(FullPath, integer_to_binary(ID)),

  case filelib:is_regular(FullName) of
    true ->
      case file:read_file(FullName) of
        {ok, Data} ->
          Data;
        {error, Reason} ->
          {error, Reason}
      end;
    false ->
      {error, no_such_file}
  end.

-spec delete(Key :: any()) -> ok | {error, Reason :: any()}.
delete({Name, ID}) ->
  FullPath = dirname(Name),
  FullName = filename:join(FullPath, integer_to_binary(ID)),
  case filelib:is_regular(FullName) of
    true ->
      file:delete(FullName);
    false ->
      {error, no_such_file}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

write_file(Path, ID, Chunk) ->
  file:write_file(filename:join([Path, integer_to_binary(ID)]), Chunk).

dirname(Name) ->
  Path = application:get_env(es3, storage_path, "/tmp/chunks/"),
  file:make_dir(Path),

  filename:join([Path, Name]).
