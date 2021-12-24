% rabbit_pub_account_log.erl
% demo_send.erl
% rabbit_send.erl
%% gen_server代码模板

-module(rabbit_pub_account_log).

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
% --------------------------------------------------------------------
-export([]).

% gen_server callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([status/0]).

-record(state, { 
	channel
	, conn
    }).

% -define(TIMER, 2000).
-define(TIMER, 1000).

-export([send/0, send/1]).
-export([pub_test/0]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("glib/include/log.hrl").
-include_lib("sys_log/include/write_log.hrl").

send() -> 
	Message = <<"info: Hello World!">>,
	send(Message).

% rabbit_pub_account_log:pub_test().
pub_test() -> 
	lists:foreach(fun(I) ->
		?LOG({pub_test, I}), 
		Params = [
			{<<"id">>, I}
		],
		send(term_to_binary({test, jsx:encode(Params)}))
	end, lists:seq(1, 50)),
	ok.

% send1() ->
% 	TestMsg = #'TestMsg'{
% 	            name = <<"jim green">>,
% 	            nick_name = <<"nick_name123456">>,
% 	            phone = <<"15912341234">> 
% 	        },
% 	TestMsgBin = msg_proto:encode_msg(TestMsg),
% 	Package = glib:package(123456, TestMsgBin),
% 	send(Package).

status() -> 
	gen_server:call(?MODULE, status).

send(Message) -> 
	gen_server:cast(?MODULE, {send, Message}).


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
	erlang:send_after(?TIMER, self(), maybe_connect),
	process_flag(trap_exit, true),
	% Channel = 
	% case maybe_connect() of 
	% 	{ok, Channel} ->  
	% 		State = #state{channel = Channel},
	% 		{ok,  State};
	% 	_ -> 
	% 		State = #state{channel = conn_fail},
	% 		{ok,  State}
	% end.

	% case maybe_connect() of 
	% 	{ok, Channel, Connection} ->  
	% 		State = #state{channel = Channel, conn = Connection},
	% 		{ok,  State};
	% 	_ -> 
	% 		State = #state{channel = conn_fail, conn = conn_fail},
	% 		{ok,  State}
	% end.

	State = #state{channel = conn_fail, conn = conn_fail},
	{ok,  State}.


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
handle_call(status, _From, State = #state{channel = Channel, conn = Connection}) ->
    case erlang:is_pid(Channel) andalso glib:is_pid_alive(Channel) 
    	andalso erlang:is_pid(Connection) andalso glib:is_pid_alive(Connection) of 
		true -> 
			 {reply, true, State};
		_ ->
			case maybe_connect() of 
				{ok, Channel1, Connection1} -> 
					NewState = #state{channel = Channel1, conn = Connection1},
					 {reply, true, NewState};
				_ ->
					{reply, false, State}
			end
	end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast({send, Message}, State=#state{channel=Channel}) ->
	% case erlang:is_pid(Channel) andalso glib:is_pid_alive(Channel) of 
	% 	true -> 
	% 		amqp_channel:cast(Channel,
	% 	                      #'basic.publish'{
	% 	                        exchange = <<"">>,
	% 	                        routing_key = queue_name()},
	% 	                      #amqp_msg{payload = Message, props=#'P_basic'{delivery_mode=2}}),
	% 		{noreply, State};
	% 	_ ->
	% 		% Channel1 = maybe_connect(),
	% 		case maybe_connect() of 
	% 			{ok, Channel1} ->
	% 				%% 重连成功，直接pub
	% 				amqp_channel:cast(Channel1,
	% 			                      #'basic.publish'{
	% 			                        exchange = <<"">>,
	% 			                        routing_key = queue_name()},
	% 			                      #amqp_msg{payload = Message}),
	% 				ok,
	% 				NewState = #state{channel = Channel1},
	% 				{noreply, NewState};
	% 			_ ->
	% 				% 重连失败， 必须将数据缓存起来，等待mq可用的时候再pub
	% 				ets_cache_pub_account_log_error:insert(Message),
	% 				{noreply, State}
	% 		end
	% end;
	NewState = try_pub(Message, State),
	{noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}           %          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_info(maybe_connect, State = #state{channel = _Channel, conn = _Connection}) ->
	% _TRef = erlang:send_after(?TIMER, self(), maybe_connect),
	case maybe_connect() of 
		{ok, Channel1, Connection1} ->  
			State1 = #state{channel = Channel1, conn = Connection1},
			{noreply,  State1};
		_ -> 
			{noreply, State}
	end;

% handle_info(maybe_connect, State = #state{channel = Channel, conn = Connection}) ->
% 	_TRef = erlang:send_after(?TIMER, self(), maybe_connect),

%     	case erlang:is_pid(Channel) andalso glib:is_pid_alive(Channel)
%     		andalso erlang:is_pid(Connection) andalso glib:is_pid_alive(Connection) of 
%     		true ->
%     			% {noreply, State};
%     			Info = amqp_connection:info(Connection, [is_closing]),
% 			    % ?LOG(Info),
% 			    case Info of 
% 			    	[{is_closing,false}|_] ->
% 			    		R = amqp_connection:info(Connection, [num_channels]),
%     					% ?LOG(R),
%     					case R of 
% 			    			[{num_channels,1}|_] ->
% 				    			% ?LOG(Info),
% 	    						{noreply, State};
% 	    					_ -> 
% 	    						glib:write_req({?MODULE, ?LINE, {Channel, Connection}}, "pubAccountLog-reconnect"),
% 		    					% ?LOG(Info),
% 		    					amqp_channel:close(Channel),
% 				    			amqp_connection:close(Connection),

% 				    			case maybe_connect() of 
% 									{ok, Channel1, Connection1} ->  
% 										State1 = #state{channel = Channel1, conn = Connection1},
% 										{noreply,  State1};
% 									_ -> 
% 										{noreply, State}
% 								end
% 						end;			
%     				_ -> 
%     					glib:write_req({?MODULE, ?LINE, {Channel, Connection}}, "pubAccountLog-reconnect"),
%     					% ?LOG(Info),
%     					amqp_channel:close(Channel),
% 		    			amqp_connection:close(Connection),

% 		    			case maybe_connect() of 
% 							{ok, Channel1, Connection1} ->  
% 								State1 = #state{channel = Channel1, conn = Connection1},
% 								{noreply,  State1};
% 							_ -> 
% 								{noreply, State}
% 						end
% 				end;
%     		_ -> 
%     			glib:write_req({?MODULE, ?LINE, {Channel, Connection}}, "pubAccountLog-reconnect"),
%     			amqp_channel:close(Channel),
%     			amqp_connection:close(Connection),

%     			case maybe_connect() of 
% 				{ok, Channel1, Connection1} ->  
% 					State1 = #state{channel = Channel1, conn = Connection1},
% 					{noreply,  State1};
% 				_ -> 
% 					{noreply, State}
% 			end
%     	end;

% handle_info(maybe_connect, State = #state{channel = Channel}) ->
%     _TRef = erlang:send_after(?TIMER, self(), maybe_connect),
%     	case erlang:is_pid(Channel) andalso glib:is_pid_alive(Channel) 
%     		of 
%     		true ->
%     			{noreply, State};
%     		_ -> 
%     			case maybe_connect() of 
% 				{ok, Channel1} ->  
% 					State1 = #state{channel = Channel1},
% 					{noreply,  State1};
% 				_ -> 
% 					{noreply, State}
% 			end
%     	end;
          % {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

% handle_info(Info, State) ->
%     % 接收来自go 发过来的异步消息
%     io:format("~nhandle info BBB!!============== ~n~p~n", [Info]),
%     {noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% priv ======================================
try_pub(Message, State=#state{channel=Channel}) ->
	case erlang:is_pid(Channel) andalso glib:is_pid_alive(Channel) of 
		true -> 
			% R = amqp_channel:cast(Channel,
		 %                      #'basic.publish'{
		 %                        exchange = <<"">>,
		 %                        routing_key = queue_name()},
		 %                      #amqp_msg{payload = Message, props=#'P_basic'{delivery_mode=2}}),
			 R =   amqp_channel:cast(Channel,
			                          #'basic.publish'{exchange = exchange()},
			                          #amqp_msg{payload = Message}),   

			% glib:write_req({?MODULE, ?LINE, Message, {pub_reply, R}}, "modifyUserAccountAndUpdateGameRecord-mq-pub"),
			?WRITE_JSON("pub_account_log", Message),

			State;
		_ ->
			% Channel1 = maybe_connect(),
			case maybe_connect() of 
				{ok, Channel1, Connection1} ->
					%% 重连成功，直接pub
					% R = amqp_channel:cast(Channel1,
				 %                      #'basic.publish'{
				 %                        exchange = <<"">>,
				 %                        routing_key = queue_name()},
				 %                      #amqp_msg{payload = Message}),

					% ok,
					R =   amqp_channel:cast(Channel1,
				                          	#'basic.publish'{exchange = exchange()},
					                          #amqp_msg{payload = Message}),   

					% glib:write_req({?MODULE, ?LINE, Message, {pub_reply, R}}, "modifyUserAccountAndUpdateGameRecord-mq-pub1"),
					?WRITE_JSON("pub_account_log_1", Message),

					NewState = #state{channel = Channel1, conn = Connection1},
					NewState;
				_ ->
					% 重连失败， 必须将数据缓存起来，等待mq可用的时候再pub
					% glib:write_req({?MODULE, ?LINE, Message}, "modifyUserAccountAndUpdateGameRecord-mq-ets"),
					?WRITE_JSON("pub_account_log_2", Message),
					
					ets_cache_pub_account_log_error:insert(Message),
					State
			end
	end.


queue_name() ->
	<<"data.account_log">>.

exchange() -> 
	<<"account_log">>.


maybe_connect() ->
	{Host1, UserName1, Password1, VirtualHost1, Port1} = case  sys_config:get_config(rabbitmq) of
	        {ok, Config} -> 
	            {_, {host, Host}, _} = lists:keytake(host, 1, Config),
	            {_, {port, Port}, _} = lists:keytake(port, 1, Config),
	            {_, {username, UserName}, _} = lists:keytake(username, 1, Config),
	            {_, {password, Password}, _} = lists:keytake(password, 1, Config),
	            {_, {virtual_host, VirtualHost}, _} = lists:keytake(virtual_host, 1, Config),

	            {glib:to_str(Host)
	            ,glib:to_binary(UserName)
	            , glib:to_binary(Password)
	            , glib:to_binary(VirtualHost)
	            ,glib:to_integer(Port)};  
	        _ -> 
	            {"localhost", <<"admin">>, <<"admin">>, <<"/">>}
	end,

	
	case amqp_connection:start(#amqp_params_network{
		heartbeat = 60,
		virtual_host = VirtualHost1,
		host = Host1,
		port = Port1,
		username           = UserName1,
	    password           = Password1
		,ssl_options = [{verify, verify_none }
			, {server_name_indication, disable}
			, {versions, ['tlsv1.3', 'tlsv1.2', 'tlsv1.1', 'tlsv1']}
			]
		% , ssl_options = [
		% 			% {cacertfile, "/path/to/ca_certificate.pem"},
        %             % {certfile,   "/path/to/server_certificate.pem"},
        %             % {keyfile,    "/path/to/server_key.pem"},
        %             {verify,     verity_none}
        %             % , {fail_if_no_peer_cert, true}
		% 			]

	}) of 
		{ok, Connection} ->
			?LOG(account_log),
			{ok, Channel} = amqp_connection:open_channel(Connection),

			% amqp_channel:call(Channel, #'exchange.declare'{exchange = exchange(),
		 %                                                   type = <<"fanout">>,
		 %                                                   durable = true}),

			% amqp_channel:call(Channel, #'queue.declare'{queue = <<"test_queue">>}),
			% amqp_channel:call(Channel, #'queue.declare'{
			%  	queue = queue_name(),
			%  	% passive= false,
			%  	durable = true
			%  	% exclusive = false,
			%  	% auto_delete = false,
			%  	% nowait = false
			%  }),
    			amqp_channel:call(Channel, #'exchange.declare'{exchange = exchange(),
                                                   type = <<"direct">>,
                                                   durable = true}),			

			{ok, Channel, Connection};
		_ -> 
			conn_fail
	end.
