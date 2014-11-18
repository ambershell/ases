%%%-------------------------------------------------------------------
%%% @author mne
%%% @copyright (C) 2013, mne
%%% @doc
%%%
%%% @end
%%% Created : 2013-06-04 00:06:24.099041
%%%-------------------------------------------------------------------
-module(ases).

-behaviour(gen_server).

%% API

-export([open/1]).
-export([close/2]).
-export([write/1]).
-export([set/2]).
-export([get/1]).
-export([dispatch/2]).
-export([stream_list/1]).
-export([new/2]).
-export([new/1]).
-export([on/1]).

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

-define(tab, begin {_Pid, Tab} = ets:lookup(?tab_list, self()), Tab end).
%%%===================================================================
%%% API
%%%===================================================================
open(Id) ->
	ets:insert(?tab, {id, Id}).

close(Id, Name) ->
	Tab = ?tab,
	case lists:keyfind(Id, 2, ets:lookup(Tab, id)) of
		false -> nil;
		ConnectLink -> ets:delete_object(Tab, ConnectLink)
	end,
	case lists:keyfind(Name, 2, ets:lookup(?tab_fibers, Id)) of
		false -> nil;
		Fiber -> ets:delete_object(?tab_fibers, Fiber)
	end.

write(Msg) ->
	[ases_messages:send(Id, Msg) || {_key, Id} <- ets:lookup(?tab, id)].

set(Key, Val) ->
	ets:delete_object(?tab, {Key, ases:get(Key)}),
	ets:insert(?tab, {Key, Val}).

get(Key) ->
	[{_Key, Val}] = ets:lookup(?tab, Key),
	Val.

dispatch(Id, [Stream | Msg]) ->
	case stream(Stream, Id) of
		nil -> {error, stream_not_found};
		Pid -> Pid ! Msg, ok
	end.

on(Event) ->
	ases_connections:on(Event).

stream_list(Id) ->
	[Name || {Name, _Pid} <- ets:tab2list(?tab_streams)] ++
	[Name || {_Id, Name, _Pid} <- ets:lookup(?tab_fibers, Id)].

new(Name, IdList) ->
	Tab = ?ets_stream,
	ets:insert(?tab_list, {self(), Tab}),
	[etc:insert(Tab, {id, Id}) || Id <- IdList],
	[etc:insert(?tab_fibers, {Id, Name, self()}) || Id <- IdList].

new(Name) ->
	ets:insert(?tab_streams, {Name, self()}),
	ets:insert(?tab_list, {self(), ?ets_stream}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	on(connection_nil),
	{ok, ok}.

handle_call(Request, From, State) ->
	?wtf_call,
	{noreply, State}.

handle_cast(Request, State) ->
	?wtf_cast,
	{noreply, State}.

handle_info({connection_nil, {Id, _Type}}, _State) ->
	ets:delete(?tab_fibers, Id),
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
stream(Name, Id) ->
	case ets:lookup(?tab_streams, Name) of
		[{_Name, Pid}] -> Pid;
		[] ->
			case lists:keyfind(Name, 2, ets:lookup(?tab_fibers, Id)) of
				{_Id, _Name, Pid} -> Pid;
				false -> nil
			end
	end.

