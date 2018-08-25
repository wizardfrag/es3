-module(es3_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
init([]) ->
  SupFlags = #{strategy => one_for_all, intensity => 50, period => 300},
  ChildSpecs = [
  ],
  {ok, {SupFlags, ChildSpecs}}.
