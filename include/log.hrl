-define(log(Name, Value),
	io:format("~n~w: ~b. ~p: ~p", [?MODULE, ?LINE, Name, Value])).

-define(sep, io:format(
	"~n=================================================================~n")).

-define(headq(HeadName),
	?sep,
	io:format("???===--->               ~p???", [HeadName])
).

-define(headb(HeadName),
	?sep,
	io:format("!!!===--->               ~p!!!", [HeadName])
).

-define(wtf_call,
	?headq("WTF CALL???"),
	?log("Request", Request),
	?log("From", From),
	?log("State", State),
	?sep
).

-define(wtf_cast,
	?headq("WTF CAST???"),
	?log("Request", Request),
	?log("State", State),
	?sep
).

-define(wtf_info,
	?headq("WTF INFO???"),
	?log("Info", Info),
	?log("State", State),
	?sep
).

-define(log_terminate,
	?headb("TERMINATE"),
	?log("Reason", Reason),
	?log("State", State),
	?sep
).

-define(wtf_ws_handle,
	?headq("WTF WebSocket HANDLE???"),
	?log("Frame", Frame),
	?log("State", State),
	?log("user-agent", cowboy_req:header(<<"user-agent">>, Req)),
	?sep
).

-define(wtf_ws_info,
	?headq("WTF WebSocket INFO???"),
	?log("Msg", Msg),
	?log("State", State),
	?log("user-agent", cowboy_req:header(<<"user-agent">>, Req)),
	?sep
).

-define(log_terminate_ws,
	?headb("TERMINATE"),
	?log("Reason", Reason),
	?log("State", State),
	?log("user-agent", cowboy_req:header(<<"user-agent">>, Req)),
	?sep
).

