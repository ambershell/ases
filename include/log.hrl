-include_lib("alog/include/alog_pt.hrl").

-define(log(Name), ?DBG("~n~p~n", Name)).
-define(log(Name, Value), ?DBG("~n~p: ~p~n", [Name, Value])).
-define(err(Name), ?ERROR("~n~p~n", Name)).
-define(err(Name, Value), ?ERROR("~n~p: ~p~n", [Name, Value])).

-define(sep, "~n=============================================================").
-define(headq(HeadName), "~n" ++ ?sep ++ "~n???===--->               " ++ HeadName ++ "???").
-define(headb(HeadName), "~n" ++ ?sep ++ "~n!!!===--->               " ++ HeadName ++ "!!!").

-define(wtf_call,
	?ERROR(?headq("WTF CALL???")
		++ "~nRequest: ~p"
		++ "~nFrom: ~p"
		++ "~nState: ~p"
		++ ?sep ++ "~n",
		[Request, From, State], 'WTF'
	)
).

-define(wtf_cast,
	?ERROR(?headq("WTF CAST???")
		++ "~nRequest: ~p"
		++ "~nState: ~p"
		++ ?sep ++ "~n",
		[Request, State], 'WTF'
	)
).

-define(wtf_info,
	?ERROR(?headq("WTF INFO???")
		++ "~nInfo: ~p"
		++ "~nState: ~p"
		++ ?sep ++ "~n",
		[Info, State], 'WTF'
	)
).

-define(log_terminate,
	?DBG(?headb("TERMINATE")
		++ "~nReason: ~p"
		++ "~nState: ~p"
		++ ?sep ++ "~n",
		[Reason, State], 'TERMINATE'
	)
).

-define(wtf_ws_handle,
	?ERROR(?headq("WTF WebSocket HANDLE???")
		++ "~nFrame: ~p"
		++ "~nState: ~p"
		++ "~nUser Agent: ~p"
		++ ?sep ++ "~n",
		[Frame, State, cowboy_req:header(<<"user-agent">>, Req)], 'WTF'
	)
).

-define(wtf_ws_info,
	?ERROR(?headq("WTF WebSocket INFO???")
		++ "~nMessage: ~p"
		++ "~nState: ~p"
		++ "~nUser Agent: ~p"
		++ ?sep ++ "~n",
		[Msg, State, cowboy_req:header(<<"user-agent">>, Req)], 'WTF'
	)
).

-define(log_terminate_ws,
	?DBG(?headb("TERMINATE")
		++ "~nReason: ~p"
		++ "~nState: ~p"
		++ "~nUser Agent: ~p"
		++ ?sep ++ "~n",
		[Reason, State, cowboy_req:header(<<"user-agent">>, Req)], 'TERMINATE'
	)
).

