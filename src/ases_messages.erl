%%%-------------------------------------------------------------------
%%% @author mne
%%% @copyright (C) 2013, mne
%%% @doc
%%%
%%% @end
%%% Created : 2013-06-04 00:06:24.099041
%%%-------------------------------------------------------------------
-module(ases_messages).

-behaviour(gen_server).

%% API
-export([send/2]).
-export([get/1]).
-export([clear/2]).

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

-define(events, [
	connection_nil
]).

-define(on(Event), ases_connections:on(Event)).
-define(inc(Id), ases_counters:inc(Id)).
-define(connect(Id), ases_connections:obj(Id)).
-define(insert(Id, Msg), ets:insert(?tab_messages, {Id, ?inc(Id), Msg})).
-define(lookup(Id), ets:lookup(?tab_messages, Id)).
-define(delete(Id), ets:delete(?tab_messages, Id)).
-define(delete_obj(Obj), ets:delete_obj(?tab_messages, Obj)).

%%%===================================================================
%%% API
%%%===================================================================

send(Id, Msg) ->
	case ?connect(Id) of
		{_Id, Pid, _Type, online, _Time} -> ?insert(Id, Msg), Pid ! delivery;
		{_Id, _Pid, _Type, offline, _Time} -> ?insert(Id, Msg);
		nil -> nil
	end.

get(Id)->
	[[Counter | Msg] || {_Id, Counter, Msg} <- ?lookup(Id)].

clear(Id, Last)->
	[?delete_obj(Obj) || {_Id, Counter, _Msg} = Obj
		<- ?lookup(Id), Counter =< Last ].

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	[?on(E) || E <- ?events],
	{ok, ok}.

handle_call(Request, From, State) ->
	?wtf_call,
	{noreply, State}.

handle_cast(Request, State) ->
	?wtf_cast,
	{noreply, State}.

handle_info({connection_nil, {Id, _Type}}, _State) ->
	?delete(Id),
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
