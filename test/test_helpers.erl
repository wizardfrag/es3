-module(test_helpers).

%% API
-export([
  random_slave/1,
  start/0,
  object_url/1,
  retry/3,
  wait_for_nodes/1]).

-define(NODES, [{es3sl1, "8091"}, {es3sl2, "8092"}, {es3sl3, "8093"}]).
-define(CONFIG_FILE, "../../../../config/test.config").

random_slave(Config) ->
  Nodes = proplists:get_value(nodes, Config, nodes()),
  V = rand:uniform(length(Nodes)),
  lists:nth(V, Nodes).

start() ->
  file:make_dir("/tmp/mnesia"),
  CodePath = code:get_path(),
  Path = lists:concat(lists:join(" ", CodePath)),

  Nodes = lists:foldl(fun({Slave, Port}, Acc) ->
    ct:pal("Starting ~p on localhost:~p...", [Slave, Port]),
    ErlFlags = binary_to_list(iolist_to_binary(io_lib:format("-config ~s -pa ~s -mnesia dir '\"/tmp/mnesia/~s\"'", [?CONFIG_FILE, Path, Slave]))),
    {ok, NodeName} = ct_slave:start(Slave, [
      {kill_if_fail, true}, {erl_flags, ErlFlags},
      {monitor_master, true},  {init_timeout, 10000},
      {startup_timeout, 10000}, {env, [{"HTTP_PORT", Port}]}
    ]),

    pong = net_adm:ping(NodeName),
    [NodeName | Acc]
                      end, [], ?NODES),

  lists:foldl(fun(Slave, _) ->
    {ok, _} = rpc:call(Slave, application, ensure_all_started, [es3])
              end, [], Nodes),
  Nodes.

random_slave_port() ->
  V = rand:uniform(length(?NODES)),
  {_, Port} = lists:nth(V, ?NODES),
  Port.

object_url(Name) when is_binary(Name) ->
  ["http://localhost:", random_slave_port(), "/objects/", Name].

retry(Fun, Expected, Attempts) ->
  retry(Fun, Expected, Attempts, 1).


retry(_Fun, _Expected, Attempts, Attempts) ->
  {error, retry_limit_reached};
retry(Fun, Expected, Attempts, _MaxAttempts) ->
  Result = Fun(),
  case Result of
    Expected ->
      Expected;
    _ ->
      ct:pal("Attempt ~p failed - got ~p expected ~p", [Attempts, Result, Expected]),
      ct:sleep(500),
      retry(Fun, Expected, Attempts+1)
  end.

wait_for_nodes(Nodes) ->
  lists:foldl(fun(Node, Acc) ->
    {error, not_found} = retry(fun() ->
      rpc:call(Node, metadata, get, [<<"NotFound">>])
               end, {error, not_found}, 3),
    [ok | Acc]
              end, [], Nodes).
