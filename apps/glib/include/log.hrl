%% rabbit 先从　rabbit:start(normal, []). 启动
%%　第一:　实始化一些　actor,  ets, mnesia 表　
%%　第二:　启动　ranch　相关的网络包,至此将启动状态设置为　ready, 说明已经准备好接收来自客户端的连接请求了　.

%% connect 
% -define(LOG(X), io:format("~n==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG(X), true).

%% data receive && login
% -define(LOG1(X), io:format("==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG1(X), true).


% -define(LOG2(X), io:format("==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG2(X), true).

-define(LOG3(X), io:format("~n==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
% -define(LOG3(X), true).


-define(LOG_START(X), io:format("~n==========log start begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
% -define(LOG_START(X), true).

-define(LOG_CLIENT_REQ(X), io:format("~n==========log LOG_CLIENT_REQ begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
% -define(LOG_CLIENT_REQ(X), true).

-define(LOG_SUB(X), io:format("~n==========log LOG_SUB begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
% -define(LOG_SUB(X), true).

%% =====================================================
%% =====================================================
%% =====================================================
%% =====================================================
%% =====================================================

%% 下面四条日志过后就建立了稳定的连接

-define(LOG_REQ(X), io:format("==========log request begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
% -define(LOG_REQ(X), true).

% ==========log request begin========{rabbit_reader,1217}==============
% #{'MethodName' => 'connection.start_ok',
%   'P1' =>
%       {'connection.start_ok',[{<<"product">>,longstr,<<"RabbitMQ">>},
%                               {<<"version">>,longstr,<<"3.9.11">>},
%                               {<<"platform">>,longstr,<<"Erlang">>},
%                               {<<"copyright">>,longstr,
%                                <<"Copyright (c) 2007-2021 VMware, Inc. or its affiliates.">>},
%                               {<<"information">>,longstr,
%                                <<"Licensed under the MPL.  See https://www.rabbitmq.com/">>},
%                               {<<"capabilities">>,table,
%                                [{<<"publisher_confirms">>,bool,true},
%                                 {<<"exchange_exchange_bindings">>,bool,true},
%                                 {<<"basic.nack">>,bool,true},
%                                 {<<"consumer_cancel_notify">>,bool,true},
%                                 {<<"connection.blocked">>,bool,true},
%                                 {<<"authentication_failure_close">>,bool,
%                                  true}]}],
%                              <<"PLAIN">>,
%                              <<0,103,117,101,115,116,0,103,117,101,115,116>>,
%                              <<"en_US">>},
%   'Protocol' => rabbit_framing_amqp_0_9_1}

% ==========log request begin========{rabbit_reader,1217}==============
% #{'MethodName' => 'connection.tune_ok',
%   'P1' => {'connection.tune_ok',2047,131072,60},
%   'Protocol' => rabbit_framing_amqp_0_9_1}

% ==========log request begin========{rabbit_reader,1217}==============
% #{'MethodName' => 'connection.open',
%   'P1' => {'connection.open',<<"/">>,<<>>,false},
%   'Protocol' => rabbit_framing_amqp_0_9_1}

% -define(LOG_START_OK(X), io:format("==========log start_ok ========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG_START_OK(X), true).

% -define(LOG_TUNE_OK(X), io:format("==========log tune_ok ========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG_TUNE_OK(X), true).


% -define(LOG_OPEN(X), io:format("==========log open ========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG_OPEN(X), true).

%% =====================================================
%% =====================================================
%% =====================================================
%% =====================================================
%% =====================================================
% -define(LOG_FRAME_REQ(X), io:format("==========********log frame request ******========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG_FRAME_REQ(X), true).

% ==========********log frame request ******========{rabbit_reader,1003}==============
% #{'Channel' => 1,'Frame' => {method,'channel.open',<<0>>}}
% -define(LOG_CREATE_CHANNEL(X), io:format("==========log create channel ========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG_CREATE_CHANNEL(X), true).
% rabbit_pub_account_log:pub_test().

% -define(LOG_CHANNEL_METHOD_CALL(X), io:format("==========log channel method call ========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG_CHANNEL_METHOD_CALL(X), true).

% ==========********log frame request ******========{rabbit_reader,1003}==============
% #{'Channel' => 1,
%   'Frame' =>
%       {method,'queue.declare',
%               <<0,0,16,100,97,116,97,46,97,99,99,111,117,110,116,95,108,111,
%                 103,2,0,0,0,0>>}}

% ==========********log frame request ******========{rabbit_reader,1003}==============
% #{'Channel' => 1,
%   'Frame' =>
%       {method,'exchange.declare',
%               <<0,0,11,97,99,99,111,117,110,116,95,108,111,103,6,100,105,114,
%                 101,99,116,2,0,0,0,0>>}}

% ==========********log frame request ******========{rabbit_reader,1003}==============
% #{'Channel' => 1,
%   'Frame' =>
%       {method,'queue.bind',
%               <<0,0,16,100,97,116,97,46,97,99,99,111,117,110,116,95,108,111,
%                 103,11,97,99,99,111,117,110,116,95,108,111,103,0,0,0,0,0,0>>}}

% ==========********log frame request ******========{rabbit_reader,1003}==============
% #{'Channel' => 1,
%   'Frame' =>
%       {method,'basic.publish',
%               <<0,0,11,97,99,99,111,117,110,116,95,108,111,103,0,0>>}}

% ==========********log frame request ******========{rabbit_reader,1003}==============
% #{'Channel' => 1,'Frame' => {content_header,60,0,8,<<0,0>>}}

% ==========********log frame request ******========{rabbit_reader,1003}==============
% #{'Channel' => 1,'Frame' => {content_body,<<"{\"id\":1}">>}}



% % 1032
% channel.open &&　queue.declare　&&　exchange.declare　&&　queue.bind

% % 1028 && 1037
% basic.publish &&




