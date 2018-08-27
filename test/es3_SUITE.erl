-module(es3_SUITE).

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1]).

-export([
  test_write/1,
  test_read/1,
  test_delete/1,
  test_read_nonexistant/1]).

-include_lib("common_test/include/ct.hrl").

-define(OBJ_SIZE, 1000).
-define(NODES, [es3sl1, es3sl2, es3sl3]).
-define(NAME, <<"TestObj">>).
-define(CONFIG_FILE, "../../../../config/test.config").

all() ->
  [
    test_write,
    test_read,
    test_delete,
    test_read_nonexistant
  ].


%%====================================================================
%% Test callbacks
%%====================================================================

init_per_suite(Config) ->
  ct:pal("Starting 3 nodes attached to ~p", [node()]),
  Nodes = start(),
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

test_read_nonexistant(Config) ->
  {error, not_found} = rpc:call(random_slave(Config), es3, read, [<<"Noname">>]).

test_delete(Config) ->
  Data = crypto:strong_rand_bytes(?OBJ_SIZE),
  ok = rpc:call(random_slave(Config), es3, write, [?NAME, Data]),

  ok = rpc:call(random_slave(Config), es3, delete, [?NAME]).

%%====================================================================
%% Internal functions
%%====================================================================

random_slave(Config) ->
  Nodes = proplists:get_value(nodes, Config, nodes()),
  V = rand:uniform(length(Nodes)),
  lists:nth(V, Nodes).

start() ->
  file:make_dir("/tmp/mnesia"),
  CodePath = code:get_path(),
  Path = lists:concat(lists:join(" ", CodePath)),

  Nodes = lists:foldl(fun(Slave, Acc) ->
    ct:pal("Starting ~p...", [Slave]),
    ErlFlags = binary_to_list(iolist_to_binary(io_lib:format("-config ~s -pa ~s -mnesia dir '\"/tmp/mnesia/~s\"'", [?CONFIG_FILE, Path, Slave]))),
    {ok, NodeName} = ct_slave:start(Slave, [
      {kill_if_fail, true}, {erl_flags, ErlFlags},
      {monitor_master, true},  {init_timeout, 10000},
      {startup_timeout, 10000}
    ]),

    pong = net_adm:ping(NodeName),
    [NodeName | Acc]
              end, [], ?NODES),

  lists:foldl(fun(Slave, _) ->
    {ok, _} = rpc:call(Slave, application, ensure_all_started, [es3])
              end, [], Nodes),
  Nodes.
