-module(object_handler).

-include("metadata.hrl").

%% API
-export([
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  delete_resource/2,
  init/2,
  router/2]).

-define(HTTP_STATUS_OK, 200).
-define(HTTP_STATUS_CREATED, 201).
-define(HTTP_STATUS_BAD_REQUEST, 400).
-define(HTTP_STATUS_NOT_FOUND, 404).
-define(HTTP_STATUS_TEAPOT, 418). % Easter egg :)
-define(HTTP_STATUS_INTERNAL_SERVER_ERROR, 500).

-define(HEADERS, #{<<"Content-Type">> => <<"application/json">>}).
-define(ERROR(Reason), jsx:encode(#{error => Reason})).
-define(JSON(Obj), jsx:encode(Obj)).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

-spec allowed_methods(Req :: cowboy_req:req(), State :: any()) -> {AllowedMethods :: list(), cowboy_req:req(), any()}.
allowed_methods(Req, #{action := object} = State) ->
  {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State};
allowed_methods(Req, #{action := Action} = State) when Action == object_info orelse Action == object_list ->
  {[<<"GET">>], Req, State};
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/binary">>, router}], Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: any()) -> {ContentTypes :: list(), cowboy_req:req(), any()}.
content_types_provided(Req, State) ->
  {[{<<"application/json">>, router}, {<<"application/binary">>, router}], Req, State}.

-spec init(Req :: cowboy_req:req(), State :: any()) -> {cowboy_rest, cowboy_req:req(), any()}.
init(Req, State) ->
  {cowboy_rest, Req, State}.

-spec router(Req0 :: cowboy_req:req(), State :: any()) -> cowboy_req:req().
router(Req0, #{action := Action} = _State) ->
  Method = cowboy_req:method(Req0),
  {Status, Body, Req} = handle_route(Method, Req0, Action),
  cowboy_req:reply(Status, ?HEADERS, Body, Req).

delete_resource(Req0, #{action := object}) ->
  {Status, Body, Req} = handle_route(<<"DELETE">>, Req0, object),
  cowboy_req:reply(Status, ?HEADERS, Body, Req).

%%====================================================================
%% Route handlers
%%====================================================================

%% GET routes
handle_route(<<"GET">>, Req, object_list) ->
  % Get list of objects
  case metadata:list() of
    List0 when is_list(List0) ->
      List = lists:foldr(fun(El, Acc) ->
        [?metadata_to_map(El) | Acc]
                         end, [], List0),
      error_logger:info_msg("GET list returned ~p~n", [List]),
      {?HTTP_STATUS_OK, ?JSON(List), Req};
    {aborted, Reason} ->
      error_logger:error_msg("GET list returned error: ~p~n", [Reason]),
      {?HTTP_STATUS_INTERNAL_SERVER_ERROR, ?ERROR(<<"Unable to retrieve list of objects">>), Req}
  end;
handle_route(<<"GET">>, Req, object) ->
  % Get single object
  Name = cowboy_req:binding(name, Req),

  case Name of
    undefined ->
      ?JSON(?ERROR(<<"Filename must be specified">>));
    Name ->
      case es3:read(Name) of
        {error, not_found} ->
          {?HTTP_STATUS_NOT_FOUND, ?ERROR(<<"No such object">>), Req};
        {error, Reason} ->
          error_logger:error_msg("GET object returned error: ~p~n", [Reason]),
          {?HTTP_STATUS_INTERNAL_SERVER_ERROR, ?ERROR(<<"An error occurred while trying to retrieve the object">>), Req};
        Data when is_binary(Data) ->
          {?HTTP_STATUS_OK, Data, Req}
      end
  end;
handle_route(<<"GET">>, Req, object_info) ->
  Name = cowboy_req:binding(name, Req),
  case metadata:get(Name) of
    Metadata = #metadata{} ->
      {?HTTP_STATUS_OK, ?JSON(?metadata_to_map(Metadata)), Req};
    {error, not_found} ->
      {?HTTP_STATUS_NOT_FOUND, ?ERROR(<<"No such object">>), Req};
    {error, Reason} ->
      error_logger:error_msg("GET object returned error: ~p~n", [Reason]),
      {?HTTP_STATUS_INTERNAL_SERVER_ERROR, ?ERROR(<<"An error occurred while trying to retrieve the object">>), Req}
  end;

handle_route(<<"POST">>, Req0, object) ->
  Name = cowboy_req:binding(name, Req0),
  case cowboy_req:has_body(Req0) of
    true ->
      {ok, Body, Req} = read_body(Req0, <<>>),
      case es3:write(Name, Body) of
        ok ->
          {?HTTP_STATUS_CREATED, ?JSON(#{name => Name, status => <<"Created">>}), Req};
        {error, Reason} ->
          error_logger:error_msg("es3:write returned error: ~p~n", [Reason]),
          {?HTTP_STATUS_INTERNAL_SERVER_ERROR, ?ERROR(<<"An error occurred while trying to write the object">>), Req}
      end;
    false ->
      {?HTTP_STATUS_BAD_REQUEST, ?ERROR(<<"You must provide a request body">>), Req0}
  end;

handle_route(<<"DELETE">>, Req, object) ->
  Name = cowboy_req:binding(name, Req),
  case es3:delete(Name) of
    ok ->
      error_logger:info_msg("DELETE object ~p OK", [Name]),
      {?HTTP_STATUS_OK, ?JSON(#{status => <<"DELETED">>}), Req};
    {error, not_found} ->
      error_logger:info_msg("DELETE object ~p not found", [Name]),
      {?HTTP_STATUS_NOT_FOUND, ?ERROR(<<"No such object">>), Req};
    {error, Reason} ->
      error_logger:error_msg("DELETE object returned error: ~p~n", [Reason]),
      {?HTTP_STATUS_INTERNAL_SERVER_ERROR, ?ERROR(<<"An error occurred while trying to retrieve the object">>), Req}
  end.

read_body(Req0, Acc) ->
  case cowboy_req:read_body(Req0) of
    {ok, Data, Req} ->
      {ok, <<Acc/binary, Data/binary>>, Req};
    {more, Data, Req} ->
      read_body(Req, <<Acc/binary, Data/binary>>)
  end.
