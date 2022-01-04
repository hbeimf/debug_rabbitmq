%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_amqqueue_sup_sup).

-behaviour(supervisor2).

-export([start_link/0, start_queue_process/3]).
-export([start_for_vhost/1, stop_for_vhost/1,
         find_for_vhost/2, find_for_vhost/1]).

-export([init/1]).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("glib/include/log.hrl").

-define(SERVER, ?MODULE).

%%----------------------------------------------------------------------------

-spec start_link() -> rabbit_types:ok_pid_or_error().
%%节点启动时会调用这里,
start_link() ->
    supervisor2:start_link(?MODULE, []).

-spec start_queue_process
        (node(), amqqueue:amqqueue(), 'declare' | 'recovery' | 'slave') ->
            pid().

%% 这里是创建队列的地方, 启动方式简单说明下, 具体如下:
%% 这里启动一个监督进程 supervisor2: rabbit_amqqueue_sup
%% rabbit_amqqueue_sup 启动一个 gen_server2 : rabbit_prequeue
%% rabbit_prequeue 由于 init 函数返回指定了回调模块: rabbit_amqqueue_process, 所以具体的逻辑都得进rabbit_amqqueue_process的回调里去看

%%刚开始启动节点的时候,分布式表里有队列信息,这里也会被调用来初始化列队的回调actor,具体是在哪里触发的? 有时间得找找
%%节点启动时会调用这里,　具体触发的位置在:　rabbit_classic_queue:recover/2 ->　rabbit_classic_queue:recover_durable_queues/1
%%　从　ｖｈｏｓｔ启动一路跟过来,
%% 如果是从tcp连接那边发起声明队列的请求,并从rabbit_channel跟过来的话, StartMode = declare
%% 如果是从节点启动后,从vhost启动一路跟过来, StartMode = recovery
%% 两种方式的调用都是从 rabbit_classic_queue 模块发起的,
start_queue_process(Node, Q, StartMode) ->
%%    ?LOG_START(#{'Node' => Node, 'Q' => Q, 'StartMode' => StartMode}),
%%    ==========log start begin========{rabbit_amqqueue_sup_sup,36}==============
%%    #{'Node' => 'rabbit@maomao-VirtualBox',
%%    'Q' =>
%%        {amqqueue,{resource,<<"/">>,queue,<<"data.account_log">>},
%%        true,false,none,[],<0.3335.0>,[],[],[],undefined,undefined,[],
%%        undefined,live,0,[],<<"/">>,
%%        #{user => <<"guest">>},
%%        rabbit_classic_queue,#{}},
%%    'StartMode' => recovery}

    #resource{virtual_host = VHost} = amqqueue:get_name(Q),
    {ok, Sup} = find_for_vhost(VHost, Node),
    %%Maf = glib_tool:pid_info(Sup),
    %%?LOG_START(Maf),
    %%==========log start begin========{rabbit_amqqueue_sup_sup,50}==============
    %%{supervisor2,init,1}
    %% 不是标准的　ｏｔｐ　启动的,　上面这种方法根本找不到是哪个模块启动的,　非要装　Ｂ　,非要跟进代码才能知道就是本模块,

    %%　此处动态启动一个 supervisor2: rabbit_amqqueue_sup
    %% 这个督程会返回三个参数, 其中第二个参数 _SupPid 就是被启动的督程的 pid, 显然这个参数在此处并不是被关心的对象
    %% 第三个参数 QPid 则一个 gen_server2 工作 actor, 这个参数作为本次调用的返回,将来 pub/sub消息都要与这个actor通信,可见其重要性
    {ok, _SupPid, QPid} = supervisor2:start_child(Sup, [Q, StartMode]),

%%    Mfa = glib_tool:pid_info(Sup),
%%    Mfa1 = glib_tool:pid_info(QPid),
%%
%%    ?LOG_CHANNEL_METHOD_CALL(#{'Q' => Q, 'Node' => Node, 'StartMode' => StartMode, 'Sup' => Sup, 'QPid' => QPid, 'Mfa' => Mfa, 'Mfa1' => Mfa1}),
    %% 这个　QPid　就是在分布式表里的那个　ｐｉｄ　,　发布消息的时候就分　ｃａｓｔ到这个ａｃｔｏｒ里去,　晚点再跟跟发布消息,
    %% **　{rabbit_amqqueue_process}  是队列　QPid　所在的模块,

    %% 这里返回的是 gen_server2 工作 actor 的 pid, 有这个pid, 就可以往队列里 pub消息或者 sub消息
    QPid.

init([]) ->
%%    ?LOG_START(here),
    {ok, {{simple_one_for_one, 10, 10},
          [{rabbit_amqqueue_sup, {rabbit_amqqueue_sup, start_link, []},
            temporary, ?SUPERVISOR_WAIT, supervisor, [rabbit_amqqueue_sup]}]}}.

-spec find_for_vhost(rabbit_types:vhost()) -> {ok, pid()} | {error, term()}.
find_for_vhost(VHost) ->
    find_for_vhost(VHost, node()).

-spec find_for_vhost(rabbit_types:vhost(), atom()) -> {ok, pid()} | {error, term()}.
find_for_vhost(VHost, Node) ->
    %%  ?LOG_START({VHost, Node}),
    %%  ==========log start begin========{rabbit_amqqueue_sup_sup,62}==============
    %%{<<"/">>,'rabbit@maomao-VirtualBox'}

    {ok, VHostSup} = rabbit_vhost_sup_sup:get_vhost_sup(VHost, Node),
    case supervisor2:find_child(VHostSup, rabbit_amqqueue_sup_sup) of
        [QSup] -> {ok, QSup};
        Result -> {error, {queue_supervisor_not_found, Result}}
    end.

-spec start_for_vhost(rabbit_types:vhost()) -> {ok, pid()} | {error, term()}.
%%节点启动时会调用这里,
start_for_vhost(VHost) ->
    case rabbit_vhost_sup_sup:get_vhost_sup(VHost) of
        {ok, VHostSup} ->
            supervisor2:start_child(
              VHostSup,
              {rabbit_amqqueue_sup_sup,
               {rabbit_amqqueue_sup_sup, start_link, []},
               transient, infinity, supervisor, [rabbit_amqqueue_sup_sup]});
        %% we can get here if a vhost is added and removed concurrently
        %% e.g. some integration tests do it
        {error, {no_such_vhost, VHost}} ->
            rabbit_log:error("Failed to start a queue process supervisor for vhost ~s: vhost no longer exists!",
                             [VHost]),
            {error, {no_such_vhost, VHost}}
    end.

-spec stop_for_vhost(rabbit_types:vhost()) -> ok.
stop_for_vhost(VHost) ->
    case rabbit_vhost_sup_sup:get_vhost_sup(VHost) of
        {ok, VHostSup} ->
            ok = supervisor2:terminate_child(VHostSup, rabbit_amqqueue_sup_sup),
            ok = supervisor2:delete_child(VHostSup, rabbit_amqqueue_sup_sup);
        %% see start/1
        {error, {no_such_vhost, VHost}} ->
            rabbit_log:error("Failed to stop a queue process supervisor for vhost ~s: vhost no longer exists!",
                             [VHost]),
            ok
    end.
