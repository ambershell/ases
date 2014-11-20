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

