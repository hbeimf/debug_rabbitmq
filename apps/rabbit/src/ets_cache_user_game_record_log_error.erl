% ets_cache_user_game_record_log_error.erl
-module(ets_cache_user_game_record_log_error).
-compile(export_all).

-include_lib("glib/include/log.hrl").
-include_lib("rabbit/include/type.hrl").

insert(PubTerm) -> 
	table_data_list:add(?TYPE_GAME_RECORD_LOG_1002, PubTerm).
	
select() ->
	select(10).

select(Limit) -> 
	table_data_list:select_by_type(?TYPE_GAME_RECORD_LOG_1002, Limit).

delete(Id) ->
	table_data_list:delete(Id).

	
