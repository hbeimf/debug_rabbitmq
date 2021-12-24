% glib_find_actor.erl

-module(glib_find_actor2).
-compile(export_all).
-include_lib("glib/include/log.hrl").


-define(LOG_1(X), io:format("~n==========log========{~p,~p}==============~n~p~n", [?MODULE,?LINE,X])).
% -define(LOG(X), true).

% % 

% % glib_find_actor:info().
% info() ->
% 	Pids = erlang:processes(),
% 	% lists:foreach(fun(Pid) -> 
% 	% 	Info = erlang:process_info(Pid),
% 	% 	% erlang:process_display(Pid, backtrace),
% 	% 	?LOG(Info),
% 	% 	ok
% 	% end, Pids),

% 	lists:foldr(fun(Pid, R) -> 
% 		Info = erlang:process_info(Pid),
% 		?LOG(Info),
% 		R

% 	end, [], Pids),
% 	ok.



% % glib_find_actor:count().
% count() -> 
% 	Num = erlang:system_info(process_count),
% 	Pids = erlang:processes(),

% 	% [P0|_] = Pids,

% 	[P1, P2, P3, P4|_] = lists:reverse(Pids),
% 	% LastPid = lists:last(Pids),
% 	?LOG({Pids, Num, P1, P2, P3}),


% 	lists:foldl(fun(P, R) -> 
% 		% erlang:process_display(P, backtrace),
% 		Info = erlang:process_info(P),
% 	% 	% erlang:process_display(Pid, backtrace),
% 		?LOG({Info, P}),
% 		R
% 	end, [], [P1,P2,P3, P4]),

% 	ok.

% glib_find_actor2:info().
info() -> 
	Mem = count1(),
	?LOG_1(Mem),
	ok.

% glib_find_actor2:mem().
mem() -> 
	Mem = count1(),
	Mem2 = erlang:memory(),
	#{m1 => Mem, m2 => Mem2}.

% glib_find_actor2:count1().
count1() -> 
	Num = erlang:system_info(process_count),
	Pids = erlang:processes(),

	% [P0|_] = Pids,

	% [P1, P2, P3, P4|_] = lists:reverse(Pids),
	% LastPid = lists:last(Pids),
	% ?LOG({Pids, Num, P1, P2, P3}),


	AllMod = lists:foldl(fun(P, R) -> 
		% erlang:process_display(P, backtrace),
		% Info = erlang:process_info(P, [current_function, current_location, current_stacktrace, dictionary, trace]),
		% Info = erlang:process_info(P, [initial_call, current_function, current_location, current_stacktrace, dictionary, trace]),
		Info = erlang:process_info(P, [dictionary]),

		Dic = glib:get_by_key(dictionary, Info),
		InitMod = glib:get_by_key('$initial_call', Dic),
		% ?LOG(InitMod),
		case InitMod of 
			{M, F, A} ->
				[{M, P}|R];
			_ -> 
				R
		end

	% % 	% erlang:process_display(Pid, backtrace),
	% 	% ?LOG({Info, P}),
	% 	R
    end, [], Pids),

    % ?LOG(AllMod),
    
	AllMod1 = lists:keysort(1, AllMod),
	% ?LOG(AllMod1),

	% AllMod1 = lists:sort(AllMod),

	% % ?LOG(AllMod1),

	Group = lists:foldl(fun({M, P} = Tuple , Reply) -> 
		case Reply of 
			[] -> 
				[[Tuple]];
			[H|T] ->
				case H of 
					[{M, _}|_] ->
						[[Tuple|H]|T];
					_ -> 
						[[Tuple]|Reply]
				end
		end

	end, [], AllMod1),

	% ?LOG(Group),

	G1 = lists:foldl(fun(G, Res) ->
		[{M, P}|_] = G, 
		[#{ mod => M, num => erlang:length(G), mem => mem(G)}|Res]
	end, [], Group),

	% ?LOG_1(G1),
	% ok.

    G1.

mem(List) -> 
	lists:foldl(fun({_, P}, Reply) -> 
		[{_, M}|_] = erlang:process_info(P, [memory]),
		% ?LOG(M),
		Reply + M
	end, 0, List).

% stack_item

% process_info_item() =
%     backtrace | binary | catchlevel | current_function |
%     current_location | current_stacktrace | dictionary |
%     error_handler | garbage_collection | garbage_collection_info |
%     group_leader | heap_size | initial_call | links | last_calls |
%     memory | message_queue_len | messages | min_heap_size |
%     min_bin_vheap_size | monitored_by | monitors |
%     message_queue_data | priority | reductions | registered_name |
%     sequential_trace_token | stack_size | status | suspending |
%     total_heap_size | trace | trap_exit



% glib_find_actor:p("<0.2200.0>").
p(Pid) -> 
	% list_to_pid("<0.4.1>").
	P = erlang:list_to_pid(Pid),
	Info = erlang:process_info(P),
	?LOG(Info),
	ok.









