-module(sys_log_tool).
-compile(export_all).

% -include_lib("glib/include/log.hrl").
-include_lib("sys_log/include/write_log.hrl").


% test() -> 
% 	LogDir = glib:log_dir() ++ "log/2020-6-1-exception-log-log.txt" ,
% 	% {ok, S} = file:open(LogDir, [read]),
% 	{ok, S} = file:open(LogDir, [raw]),

% 	{ok, Str1} = file:read_line(S),
% 	?LOG(Str1),

% 	{ok,Tokens,_} = erl_scan:string(Str1),%% 要解析的字符串一定要以.结尾
% 	{ok, Term} = erl_parse:parse_term(Tokens),
% 	?LOG(Term),

% 	ok.

% sys_log_tool:logs().
logs() -> 
	LogDir = glib:log_dir() ++ "log/log.md" ,
	Terms = log_2_terms(LogDir),
	% lists:foreach(fun(Term) -> 
	% 	?LOG(Term),
	% 	ok
	% end, Terms),
	ok.


log_2_terms(LogDir) -> 
	Lines = lists:reverse(read_lines(LogDir)),
	lists:foreach(fun(Line) -> 
		% {ok,Tokens,_} = erl_scan:string(Line),
		% ?LOG(Tokens),
		% {ok, Term} = erl_parse:parse_term(Tokens),
		% [Term|Reply]
		% ?LOG(Line),
		% ?LOG(Term),
		% Reply
		ok
	end,  Lines),
	ok.

read_lines(LogDir) ->
	{ok, File} = file:open(LogDir, read),
	Lines = read_lines(File, []),
	Lines.
	
read_lines(File, Reply) ->
	case io:get_line(File, "Read a line") of 
		eof ->
			Reply;
		Line -> 
			read_lines(File, [Line|Reply])
	end. 
