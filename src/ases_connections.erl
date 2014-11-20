%%%-------------------------------------------------------------------
%%% @author mne
%%% @copyright (C) 2013, mne
%%% @doc
%%%
%%% @end
%%% Created : 2013-06-04 00:06:24.099041
%%%-------------------------------------------------------------------
-module(ases_connections).

-behaviour(gen_server).

%% API
-export([connect/2]).
-export([disconnect/2]).
-export([up_connect/2]).

-export([obj/1]).
-export([pid/1]).
-export([type/1]).
-export([state/1]).

-export([add_event_listener/1]).
-export([del_event_listener/1]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("ases.hrl").
-include("def.hrl").
-include("log.hrl").

-define(ttl, 10 * 60 * 1000).
-define(ttl_mk, ?ttl * 1000).
-define(refresh_time, 60 * 1000).
-define(refresh_time_mk, ?refresh_time * 1000).

%%%===================================================================
%%% API
%%%===================================================================

connect(Id, Type) ->
	NewId = case ets:lookup(?tab_connections, Id) of
		[{_,_,_,offline,_}] ->
			fire_event(connection_on, {Id, Type}),
			Id;
		[] ->
			fire_event(connection_new, {Uid = make_uid(), Type}),
			Uid;
		_ ->
			Id
	end,
	ets:insert(?tab_connections, {NewId, self(), Type, online, now()}),
	{NewId, ?ttl}.

disconnect(Id, Type)->
	ets:insert(?tab_connections, {Id, nil, Type, offline, now()}),
	fire_event(connection_off, {Id, Type}).

up_connect(Id, Type)->
	ets:insert(?tab_connections, {Id, self(), Type, online, now()}).

obj(Id) ->
	case ets:lookup(?tab_connections, Id) of
		[] -> nil;
		[Obj] -> Obj
	end.

pid(Id) ->
	case ets:lookup(?tab_connections, Id) of
		[] -> nil;
		[{_Id, Pid, _Type, _State, _Time}] -> Pid
	end.

type(Id) ->
	case ets:lookup(?tab_connections, Id) of
		[] -> nil;
		[{_Id, _Pid, Type, _State, _Time}] -> Type
	end.

state(Id) ->
	case ets:lookup(?tab_connections, Id) of
		[] -> nil;
		[{_Id, _Pid, _Type, State, _Time}] -> State
	end.

add_event_listener(Event)->
	ets:insert(?tab_listeners, {Event, self()}).

del_event_listener(Event)->
	ets:delete_object(?tab_listeners, {Event, self()}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	{ok, ok, ?refresh_time}.

handle_call(Request, From, State) ->
	?wtf_call,
	{noreply, State, ?refresh_time}.

handle_cast(Request, State) ->
	?wtf_cast,
	{noreply, State, ?refresh_time}.

handle_info(timeout, _State) ->
	ets:foldl(fun refresh_state/2, now(), ?tab_connections),
	{noreply, ok, ?refresh_time};

handle_info(Info, State) ->
	?wtf_info,
	{noreply, State, ?refresh_time}.

terminate(Reason, State) ->
	?log_terminate,
	ok.

code_change(_OldVsn, _State, _Extra) ->
	{ok, ok}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
refresh_state({Id, Pid, Type, _State, Time}, T2) ->
	T = timer:now_diff(T2, Time),
	if
		?ttl_mk < T ->
			ets:delete(?tab_connections, Id),
			fire_event(connection_nil, {Id, Type});
		?refresh_time_mk < T ->
			case Pid of
				nil -> ok;
				_ ->
					ets:insert(?tab_connections, {Id, nil, Type, offline, Time}),
					fire_event(connection_off, {Id, Type}),
					Pid ! kill
			end;
		?refresh_time_mk >= T -> ok
	end,
	T2.

fire_event(Event, Data)->
	lists:foreach(
		fun({_Event, Pid}) -> Pid ! {Event, Data} end,
		ets:lookup(?tab_listeners, Event)
	).

make_uid() ->
	{N2b1, N2b2, N2b3} = now(),
	TimeMark = <<
		(integer_to_binary(N2b3, 36))/binary,
		(integer_to_binary(N2b1, 36))/binary,
		(integer_to_binary(N2b2, 36))/binary
	>>,
	<< <<P:8, (crypto:rand_uniform($?, $~ + 1)):8>> || <<P:8>> <= TimeMark >>.

