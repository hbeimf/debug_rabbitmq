-module(ets_cache_pub_account_log_error).
-compile(export_all).

-include_lib("glib/include/log.hrl").
-include_lib("rabbit/include/type.hrl").

insert(PubTerm) -> 
	table_data_list:add(?TYPE_ACCOUNT_LOG_1001, PubTerm).
	
select() ->
	select(10).

select(Limit) -> 
	table_data_list:select_by_type(?TYPE_ACCOUNT_LOG_1001, Limit).

delete(Id) ->
	table_data_list:delete(Id).

