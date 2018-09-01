-module(es3_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [node()]).

start_link(Nodes) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Nodes).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
init(Nodes) ->
  SupFlags = #{strategy => one_for_all, intensity => 50, period => 300},
  ChildSpecs = [
    #{
      id => metadata,
      start => {metadata ,start_link, [Nodes]},
      type => worker
    },
    #{
      id => storage,
      start => {storage_server, start_link, []},
      type => worker
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.
