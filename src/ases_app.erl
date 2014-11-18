%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ases_app).
-behaviour(application).

%% API.
-export([start/0]).
-export([start/2]).
-export([stop/1]).

%% API.
start() ->
	application:ensure_all_started(ases).

start(_Type, _Args) ->
	{ok, Listen} = application:get_env(ases, listen),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/[...]", ases_websocket_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100,
		Listen, [{env, [{dispatch, Dispatch}]}]
	),
	ases_sup:start_link().

stop(_State) ->
	ok.

