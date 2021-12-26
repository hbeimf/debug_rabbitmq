-module(glib_format).
-compile(export_all).

-include_lib("glib/include/log.hrl").
-include_lib("stdout_formatter/include/stdout_formatter.hrl").


% glib_format:to_string().
to_string() -> 
    Num = 500 / 10000,
    Three = three(Num),
    ?LOG(Three),
    Four = four(Num),
    ?LOG(Four),
    
    ok.

three(Num) ->
    stdout_formatter_paragraph:to_string(#paragraph{content = Num, props = #{format => "~.3.0f"}}).

four(Num) ->
    stdout_formatter_paragraph:to_string(#paragraph{content = Num, props = #{format => "~.4.0f"}}).
    