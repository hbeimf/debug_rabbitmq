% ets_cache_channel_credits_log.erl
-module(ets_cache_channel_credits_log).
-compile(export_all).

-include_lib("glib/include/log.hrl").
-include_lib("rabbit/include/type.hrl").

insert(PubTerm) -> 
	table_data_list:add(?TYPE_CHANNEL_CREDITS_LOG_1003, PubTerm).
	
select() ->
	select(10).

select(Limit) -> 
	table_data_list:select_by_type(?TYPE_CHANNEL_CREDITS_LOG_1003, Limit).

delete(Id) ->
	table_data_list:delete(Id).

