%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ases_sup).
-behaviour(supervisor).

-include("def.hrl").

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	?ets_set(?tab_counters),
	?ets_bug(?tab_messages),
	?ets_set(?tab_connections),
	?ets_bug(?tab_listeners),
	?ets_set(?tab_streams),
	?ets_set(?tab_list),
    ?ets_bug(?tab_fibers),

	RestartStrategy = one_for_one,
	MaxRestarts = 10,
	MaxSecondsBetweenRestarts = 10,

	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Procs = [
		?srv(ases_connections),
		?srv(ases_counters),
		?srv(ases_messages),
		?srv(ases)
	],

	{ok, {SupFlags, Procs}}.

