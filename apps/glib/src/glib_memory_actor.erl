%% gen_server代码模板

-module(glib_memory_actor).

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
% --------------------------------------------------------------------
% -export([log/6
% 	,log_json/6
% 	]).

% gen_server callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% -export([stop_rs_server/0]).

% -record(state, { 
% 	port=0
% }).

-include_lib("glib/include/log.hrl").
-include_lib("sys_log/include/write_log.hrl").

-define(MEMORY_TIMER, timer:minutes(10)).   %% 
% log(Pid, Log, Day, Time, Module, Line) -> 
% % log(Pid, Log, Day, Time) ->
% 	% ?LOG({Pid, Log}),
% 	gen_server:cast(Pid, {write, Log, Day, Time, Module, Line}),
% 	ok.

% log_json(Pid, Log, Day, Time, Module, Line) -> 
% 	gen_server:cast(Pid, {write_json, Log, Day, Time, Module, Line}),
% 	ok.

% --------------------------------------------------------------------
% External API
% --------------------------------------------------------------------
% start_link() ->
% 	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([]) ->
	% sys_log_ets:set_config(LogFile, self()),
	% % Port = start_rs_server(),
	% % State = #state{port = Port},
	% State = [LogFile],
	% ?LOG10(gc),
	erlang:send_after(?MEMORY_TIMER, self(), memory_log), % 
	State = #{},
	{ok, State}.

% --------------------------------------------------------------------
% Function: handle_call/3
% Description: Handling call messages
% Returns: {reply, Reply, State}          |
%          {reply, Reply, State, Timeout} |
%          {noreply, State}               |
%          {noreply, State, Timeout}      |
%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	% ?LOG(Request),
	Reply = ok,
	{reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
% handle_cast({write, Log, Day, Time, Module, Line}, [LogFile|_] = State) ->
% 	% ?LOG({write, Log, Day, Time, Module, Line}),
% 	sys_log:log_json(Log, LogFile, Day, Time, Module, Line),
% 	{noreply, State};
% handle_cast({write_json, Log, Day, Time, Module, Line}, [LogFile|_] = State) ->
% 	% ?LOG({write, Log, Day, Time, Module, Line}),
% 	sys_log:log_json_in_data(Log, LogFile, Day, Time, Module, Line),
% 	{noreply, State};
handle_cast(_Msg, State) ->
	% ?LOG(Msg),
	{noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
% handle_info({#Port<0.51859>,{exit_status,143}}, State) ->
% handle_info({Port, {exit_status, _}}, State=#state{port=Port}) ->
% 	?LOG(Port),
% 	NewPort = start_rs_server(),
% 	{noreply, State#state{port = NewPort}};

handle_info(memory_log, State) ->
    Log = glib_find_actor2:mem(),
    ?WRITE_LOG("memory_used_info", Log),
    erlang:send_after(?MEMORY_TIMER, self(), memory_log), % 
    {noreply, State};
% erlang:send_after(?MEMORY_TIMER, self(), memory_log), % 

handle_info(_Info, State) ->
	% ?LOG(Info),
	{noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(_Reason, _State) ->
	% stop_rs_server(),
	ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


