-module(ets_cache_pub_modify_account_interface).
-compile(export_all).

-include_lib("glib/include/log.hrl").
-include_lib("rabbit/include/type.hrl").

insert(PubTerm) -> 
	table_data_list:add(?TYPE_MODIFY_ACCOUNT_INTERFACE_1004, PubTerm).
	
select() ->
	select(10).

select(Limit) -> 
	table_data_list:select_by_type(?TYPE_MODIFY_ACCOUNT_INTERFACE_1004, Limit).

delete(Id) ->
	table_data_list:delete(Id).

