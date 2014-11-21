-include_lib("alog/include/alog_pt.hrl").

-define(log(Name), ?DBG(Name)).
-define(log(Name, Value), ?DBG("~p: ~p", [Name, Value])).
-define(err(Name), ?ERROR(Name)).
-define(err(Name, Value), ?ERROR("~p: ~p", [Name, Value])).

-define(sepd, ?DBG("=============================================================")).
-define(sepd(Tag), ?DBG("=============================================================", [], Tag)).
-define(sepe, ?ERROR("=============================================================")).
-define(sepe(Tag), ?ERROR("=============================================================", [], Tag)).
-define(headq(HeadName, Tag), begin
	?ERROR(""),
	?sepe(Tag),
	?ERROR("| ???===--->               " ++ HeadName ++ "???", [], Tag)
end).
-define(headb(HeadName, Tag), begin
	?DBG(""),
	?sepd(Tag),
	?DBG("| !!!===--->               " ++ HeadName ++ "!!!", [], Tag)
end).

-define(wtf_call, begin
	?headq("WTF CALL", 'WTF'),
	?ERROR("| Request: ~p", [ Request ], 'WTF'),
	?ERROR("| From: ~p", [ From ], 'WTF'),
	?ERROR("| State: ~p", [ State ], 'WTF'),
	?sepe('WTF'),
	?ERROR("")
end).

-define(wtf_cast, begin
	?headq("WTF CAST", 'WTF'),
	?ERROR("| Request: ~p", [ Request ], 'WTF'),
	?ERROR("| State: ~p", [ State ], 'WTF'),
	?sepe('WTF'),
	?ERROR("")
end).

-define(wtf_info, begin
	?headq("WTF INFO", 'WTF'),
	?ERROR("| Info: ~p", [ Info ], 'WTF'),
	?ERROR("| State: ~p", [ State ], 'WTF'),
	?sepe('WTF'),
	?ERROR("")
end).

-define(log_terminate, begin
	?headb("TERMINATE", 'TERMINATE'),
	?DBG("| Reason: ~p", [ Reason ], 'TERMINATE'),
	?DBG("| State: ~p", [ State ], 'TERMINATE'),
	?sepd('TERMINATE'),
	?DBG("")
end).

-define(wtf_ws_handle, begin
	?headq("WTF WebSocket HANDLE", 'WTF'),
	?ERROR("| Frame: ~p", [ Frame ], 'WTF'),
	?ERROR("| State: ~p", [ State ], 'WTF'),
	?ERROR("| User Agent: ~p", [ cowboy_req:header(<<"user-agent">>, Req)], 'WTF'),
	?sepe('WTF'),
	?ERROR("")
end).

-define(wtf_ws_info, begin
	?headq("WTF WebSocket INFO", 'WTF'),
	?ERROR("| Message: ~p", [ Msg ], 'WTF'),
	?ERROR("| State: ~p", [ State ], 'WTF'),
	?ERROR("| User Agent: ~p", [ cowboy_req:header(<<"user-agent">>, Req)], 'WTF'),
	?sepe('WTF'),
	?ERROR("")
end).

-define(log_terminate_ws, begin
	?headb("TERMINATE", 'TERMINATE'),
	?DBG("| Reason: ~p", [ Reason ], 'TERMINATE'),
	?DBG("| State: ~p", [ State ], 'TERMINATE'),
	?DBG("| User Agent: ~p", [ cowboy_req:header(<<"user-agent">>, Req)], 'TERMINATE'),
	?sepd('TERMINATE'),
	?DBG("")
end).
