% ets_error_log.erl
-module(ets_error_log).
-compile(export_all).

-include_lib("glib/include/log.hrl").

-define(ETS_OPTS,[set, public ,named_table , {keypos,2}, {heir,none}, {write_concurrency,true}, {read_concurrency,true}]).

-define(TABLE, ets_error_log).
-record(ets_error_log, {
	id,
	term
}).


% ets_error_log:init().
init() ->
	?MODULE = ets:new(?TABLE, ?ETS_OPTS),
	ok.

insert(PubTerm) ->
	Id = glib:uid(),
	ets:insert(?TABLE, #ets_error_log{id=Id, term=PubTerm}).

select() ->
	select(10).
	% ets:match_object(?TABLE, #ets_error_log{_='_', _='_'}).
	% case ets:match_object(?TABLE, #ets_error_log{_='_', _='_'}) of
	% 	% [{_, GameId, Pid}] -> {ok, Pid};
	% 	[] ->
	% 		[];
	% 	CacheList -> 
	% 		CacheList
	% end.

select(Limit) ->
	ets:match_object(?TABLE, #ets_error_log{_='_', _='_'}, Limit).



delete(Id) ->
	ets:match_delete(?TABLE, #ets_error_log{id=Id, _='_'}).
