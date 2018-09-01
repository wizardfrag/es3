-module(http_SUITE).

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1]).

-export([
  test_write/1,
  test_read/1,
  test_read_nonexistent/1,
  test_delete/1]).

-import(test_helpers, [start/0, object_url/1, wait_for_nodes/1]).

-include_lib("common_test/include/ct.hrl").


-define(OBJ_SIZE, 1000).
-define(NAME, <<"TestObj">>).

all() ->
  [
    test_write,
    test_read,
    test_read_nonexistent,
    test_delete
  ].

%%====================================================================
%% Test callbacks
%%====================================================================

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(hackney),
  Nodes = case nodes() of
    L  when length(L) > 0 ->
      L;
    _ ->
      N = start(),
      wait_for_nodes(N),
      N
  end,
  [{nodes, Nodes} | Config].

end_per_suite(Config) ->
  Config.


%%====================================================================
%% Tests
%%====================================================================

test_write(_Config) ->
  Url = object_url(?NAME),
  Data = crypto:strong_rand_bytes(?OBJ_SIZE),
  Headers = [{<<"Accept">>, <<"application/json">>}, {<<"Content-Type">>, <<"application/binary">>}],
  Options = [with_body],
  {ok, 201, _, _} = hackney:post(Url, Headers, Data, Options).

test_read(_Config) ->
  Url = object_url(?NAME),
  Data = crypto:strong_rand_bytes(?OBJ_SIZE),
  Headers = [{<<"Accept">>, <<"application/json">>}, {<<"Content-Type">>, <<"application/binary">>}],
  Options = [with_body],
  {ok, 201, _, _} = hackney:post(Url, Headers, Data, Options),

  Headers2 = [{<<"Accept">>, <<"application/json">>}],
  {ok, 200, _, Data} = hackney:get(Url, Headers2, <<>>, Options).

test_read_nonexistent(_Config) ->
  Url = object_url(<<"Noname">>),
  Headers = [{<<"Accept">>, <<"application/json">>}],
  Options = [with_body],

  {ok, 404, _, _} = hackney:get(Url, Headers, <<>>, Options).

test_delete(_Config) ->
  Url = object_url(?NAME),
  Headers = [{<<"Accept">>, <<"application/json">>}],
  Options = [with_body],
  {ok, 200, _, _} = hackney:delete(Url, Headers, <<>>, Options).
