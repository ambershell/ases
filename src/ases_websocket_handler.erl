-module(ases_websocket_handler).

-include("log.hrl").

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

-define(ok, {ok, Req, State}).
-define(shutdown, {shutdown, Req, State}).
-define(ping, {reply, {ping, Id}, Req, State}).
-define(reply, {reply, {text, jsonx:encode(Frame)}, Req, State}).
-define(idFrame, Frame = [[ases_counters:inc(Id), id, Id, TTL]]).

init(Req, _Opts) ->
	#{id := Id} = cowboy_req:match_cookies([id], Req),
	{NewId, TTL} = ases_connections:connect(Id, websocket),
	self() ! save_id,
	{cowboy_websocket, Req, {NewId, TTL}}.

websocket_handle({text, <<"ping">>}, Req, State) ->
	self() ! save_id,
	?ok;

websocket_handle({text, Json}, Req, {Id, _TTL} = State) ->
	Result = case jsonx:decode(Json) of
		{error, _, _} -> {error, bad_json};
		[<<"last">>, Last] -> ases_messages:clear(Id, Last), ok;
		[<<"stream_list">>] ->
			ases_messages:send(Id, [stream_list | ases:stream_list(Id)]), ok;
		Msg -> ases:dispatch(Id, Msg)
	end,
	case Result of
		ok -> ok;
		_ -> self() ! Result
	end,
	?log(json, Json),
	?log(term, jsonx:decode(Json)),
	?ok;

websocket_handle(Frame, Req, State) ->
	?wtf_ws_handle,
	?ok.

websocket_info(delivery, Req, {Id, _TTL} = State) ->
	Frame = ases_messages:get(Id),
	?reply;

websocket_info(save_id, Req, {Id, TTL} = State) ->
	ases_connections:up_connect(Id, websocket),
	?idFrame,
	?reply;

websocket_info(kill, Req, OldState) ->
	State = {kill, OldState},
	?shutdown;

websocket_info(Msg, Req, State) ->
	?wtf_ws_info,
	?ok.

terminate(Reason, Req, {kill, _OldState} = State) ->
	?log_terminate_ws,
	ok;

terminate(Reason, Req, {Id, _TTL} = State) ->
	ases_connections:disconnect(Id, websocket),
	?log_terminate_ws,
	ok.

% reply(Frame, Req, State) ->
% 	% OutFrame = {binary, make_uid()},
% 	OutFrame = {text, jsonx:encode(Frame)},
% 	{reply, OutFrame, Req, State, hibernate}.

% ok(Req, State) ->
% 	{ok, Req, State, hibernate}.

% websocket_handle(InFrame, Req, State) ->
% 	{ok, Req, State}
% 	{ok, Req, State, hibernate}
% 	{reply, OutFrame | [OutFrame], Req, State}
% 	{reply, OutFrame | [OutFrame], Req, State, hibernate}
% 	{shutdown, Req, State}

% websocket_info(Info, Req, State) ->
% 	{ok, Req, State}
% 	{ok, Req, State, hibernate}
% 	{reply, OutFrame | [OutFrame], Req, State}
% 	{reply, OutFrame | [OutFrame], Req, State, hibernate}
% 	{shutdown, Req, State}

