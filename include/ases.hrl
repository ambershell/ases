% ets names
-define(tab_counters,    ases_counters).
-define(tab_messages,    ases_messages).
-define(tab_connections, ases_connections).
-define(tab_listeners,   ases_event_listeners).
-define(tab_streams,     ases_streams).
-define(tab_stream,      ases_stream).
-define(tab_list,        ases_stream_list).
-define(tab_fibers,      ases_fibers).

% connection events
-define(on(Events), [ases_connections:add_event_listener(E) || E <- Events]).
-define(off(Events), [ases_connections:del_event_listener(E) || E <- Events]).
