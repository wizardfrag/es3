-module(es3_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  mnesia:stop(),
  {ok, Nodes} = application:get_env(es3, nodes),
  % We use the below so it can either be set by the config file or environment variable (override)
  EnvPort = os:getenv("HTTP_PORT"),
  HTTPPort = case EnvPort of
    false ->
      application:get_env(es3, http_port, 8080);
    _ ->
      list_to_integer(EnvPort)
  end,

  error_logger:info_msg("HTTP handler starting on port ~p~n", [HTTPPort]),

  Dispatch = cowboy_router:compile([
    {'_', [
      % {PathMatch, Handler, InitialState}
      % Get metadata info on a object
      {"/objects/:name/info", object_handler, #{action => object_info}},

      % Get, write or delete a specific object
      {"/objects/:name", object_handler, #{action => object}},

      % Get a list of all objects
      {"/objects", object_handler, #{action => object_list}}

    ]}
  ]),

  {ok, _} = cowboy:start_clear(http_listener, [{port, HTTPPort}], #{env => #{dispatch => Dispatch}}),

  es3_sup:start_link(Nodes).

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
