-module(glib_tool).
-compile(export_all).

-include_lib("glib/include/log.hrl").

% glib_tool:pid_info(Pid).
pid_info(Pid) -> 
    Info = erlang:process_info(Pid, [dictionary]),
    Dic = glib:get_by_key(dictionary, Info),
    InitMod = glib:get_by_key('$initial_call', Dic),
    % ?LOG2(InitMod),
    InitMod.
