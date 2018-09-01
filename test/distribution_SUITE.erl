-module(distribution_SUITE).

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1]).

-export([
  test_write/1,
  test_read/1,
  test_delete/1,
  test_read_nonexistent/1]).

-import(test_helpers, [start/0, random_slave/1, wait_for_nodes/1]).

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
  ct:pal("Starting 3 nodes attached to ~p", [node()]),
  Nodes = start(),
  wait_for_nodes(Nodes),
  [{nodes, Nodes} | Config].

end_per_suite(Config) ->
  Config.

%%====================================================================
%% Tests
%%====================================================================

test_write(Config) ->
  Data = crypto:strong_rand_bytes(?OBJ_SIZE),
  ok = rpc:call(random_slave(Config), es3, write, [?NAME, Data]).

test_read(Config) ->
  Data = crypto:strong_rand_bytes(?OBJ_SIZE),
  ok = rpc:call(random_slave(Config), es3, write, [?NAME, Data]),

  Data = rpc:call(random_slave(Config), es3, read, [?NAME]).

test_read_nonexistent(Config) ->
  {error, not_found} = rpc:call(random_slave(Config), es3, read, [<<"Noname">>]).

test_delete(Config) ->
  Data = crypto:strong_rand_bytes(?OBJ_SIZE),
  ok = rpc:call(random_slave(Config), es3, write, [?NAME, Data]),

  ok = rpc:call(random_slave(Config), es3, delete, [?NAME]).

