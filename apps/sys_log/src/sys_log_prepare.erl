% sys_log_prepare.erl

% sys_log.erl
% glib_log.erl
-module(sys_log_prepare).
-compile(export_all).

% -include_lib("glib/include/log.hrl").
-include_lib("sys_log/include/write_log.hrl").

-define(LOG(X), io:format("~n==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
% -define(LOG(X), true).

log_json(Json, LogFile, Day, Time, Module, Line) ->
	LogDir = glib:log_dir() ++ "log/" ++ Day ++ "-"++ glib:to_str(LogFile) ++"-log.txt",

	Log = [
		% {<<"term">>, glib:to_binary(glib:to_str(sys_log_format:format(Json)))}
		{<<"term">>, Json}
		, {<<"module">>, Module}
		, {<<"line">>, Line}
		, {<<"receive_time">>, glib:to_binary(Time)}
		, {<<"write_time">>, glib:to_binary(glib:date_str())}
	],
%%	Log = jsx:encode(Data),

	%% 同时写入文件
	append(LogDir, Log).


append(Dir, Data) ->
	append(jsx:is_json(Data), Dir, Data).

append(true, Dir, Data) ->
	% case glib:file_exists(Dir) of
	% 	true ->
	% 		file:write_file(Dir, "\n" ++ Data, [append, raw]);
	% 	_ ->
	% 		file:write_file(Dir, Data, [append, raw])
	% end;
	{Dir, Data};
append(_, Dir, Data1) ->
	% {ok, S} = file:open(Dir, [append]),
	% io:format(S, "~p.~n~n", [Data]),
	% file:close(S).

	Data = lists:concat([glib:to_str(sys_log_format:format(Data1))]),

	% case glib:file_exists(Dir) of
	%     true ->
	%       file:write_file(Dir, "\n" ++ Data, [append]);
	%     _ ->
	%       file:write_file(Dir, Data, [append])
	%   end.
	{Dir, Data}.

write_list(_, []) -> 
	ok;
write_list(Dir, LogList) ->
	% ?LOG(LogList),
	Data = glib:implode(lists:reverse(LogList), "\n"),
	case glib:file_exists(Dir) of
	    true ->
	      file:write_file(Dir, "\n" ++ Data, [append]);
	    _ ->
	      file:write_file(Dir, Data, [append])
	end.

	% ok.

