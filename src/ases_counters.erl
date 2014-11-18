%%%-------------------------------------------------------------------
%%% @author mne
%%% @copyright (C) 2013, mne
%%% @doc
%%%
%%% @end
%%% Created : 2013-06-04 00:06:24.099041
%%%-------------------------------------------------------------------
-module(ases_counters).

-behaviour(gen_server).

%% API
-export([new/1]).
-export([inc/1]).
-export([dec/1]).
-export([get/1]).
-export([delete/1]).
-export([reset/1]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("def.hrl").
-include("log.hrl").

-define(counters, [
	online_websocket,
	offline_websocket,
	online_iframe,
	offline_iframe
]).

-define(events, [
	connection_new,
	connection_on,
	connection_off,
	connection_nil
]).

-define(on(Event), ases_connections:on(Event)).

%%%===================================================================
%%% API
%%%===================================================================

get(Id) ->
	case ets:lookup(?tab_counters, Id) of
		[] -> nil;
		[{Id, Counter}]  -> Counter
	end.

inc(Id) ->
	ets:update_counter(?tab_counters, Id, 1).

dec(Id) ->
	ets:update_counter(?tab_counters, Id, -1).

delete(Id) ->
	ets:delete(?tab_counters, Id).

new(Id) ->
	ets:insert(?tab_counters, {Id, 0}).

reset(Id) ->
	ets:insert(?tab_counters, {Id, 0}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	[new(C) || C <- ?counters],
	[?on(E) || E <- ?events],
	{ok, ok}.

handle_call(Request, From, State) ->
	?wtf_call,
	{noreply, State}.

handle_cast(Request, State) ->
	?wtf_cast,
	{noreply, State}.

handle_info({connection_new, {Id, websocket}}, _State) ->
	new(Id),
	inc(online_websocket),
	{noreply, ok};

handle_info({connection_new, {Id, iframe}}, _State) ->
	new(Id),
	inc(online_iframe),
	{noreply, ok};

handle_info({connection_on, {_Id, websocket}}, _State) ->
	inc(online_websocket),
	dec(offline_websocket),
	{noreply, ok};

handle_info({connection_on, {_Id, iframe}}, _State) ->
	inc(online_iframe),
	dec(offline_iframe),
	{noreply, ok};

handle_info({connection_off, {_Id, websocket}}, _State) ->
	inc(offline_websocket),
	dec(online_websocket),
	{noreply, ok};

handle_info({connection_off, {_Id, iframe}}, _State) ->
	inc(offline_iframe),
	dec(online_iframe),
	{noreply, ok};

handle_info({connection_nil, {Id, websocket}}, _State) ->
	dec(offline_websocket),
	delete(Id),
	{noreply, ok};

handle_info({connection_nil, {Id, iframe}}, _State) ->
	dec(offline_iframe),
	delete(Id),
	{noreply, ok};

handle_info(Info, State) ->
	?wtf_info,
	{noreply, State}.

terminate(Reason, State) ->
	?log_terminate,
	ok.

code_change(_OldVsn, _State, _Extra) ->
	{ok, ok}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
