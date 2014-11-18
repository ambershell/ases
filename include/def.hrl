-define(sup(Name), {
	Name,
	{Name, start_link, []},
	permanent,
	infinity,
	supervisor,
	[Name]
}).

-define(srv(Name), {
	Name,
	{Name, start_link, []},
	permanent,
	1000,
	worker,
	[Name]
}).

%% ETS
-define(tab_counters,    ases_counters).
-define(tab_messages,    ases_messages).
-define(tab_connections, ases_connections).
-define(tab_listeners,   ases_event_listeners).
-define(tab_streams,     ases_streams).
-define(tab_stream,      ases_stream).
-define(tab_list,        ases_stream_list).
-define(tab_fibers,      ases_fibers).

-define(ets_set(Name),
	ets:new(Name, [
		set,
		public,
		named_table,
		{read_concurrency, true}
	])
).
-define(ets_bug(Name),
	ets:new(Name, [
		duplicate_bag,
		public,
		named_table,
		{read_concurrency, true}
	])
).
-define(ets_stream,
	ets:new(?tab_stream, [
		duplicate_bag,
		private
	])
).

