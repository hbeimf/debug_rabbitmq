%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_amqqueue_sup).

-behaviour(supervisor2).

-export([start_link/2]).

-export([init/1]).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("glib/include/log.hrl").
%%----------------------------------------------------------------------------

-spec start_link(amqqueue:amqqueue(), rabbit_prequeue:start_mode()) ->
          {'ok', pid(), pid()}.

%% 第一个参数 Q 包含了队列的相关元数据,用来启动列队 actor,
%% 此处先启动一个 supervisor2, 然后再在这个督程下启动一个子工作actor: gen_server2, 具体回调模块是 :rabbit_prequeue
%% 返回 {ok, SupPid, QPid}. 三个参数, SupPid 为当前督程的 pid, QPid为工作进程 pid, 这个pid的具体工作就是处理队列工作的具体逻辑.
start_link(Q, StartMode) ->
%%    ?LOG_START(#{'Q' => Q, 'StartMode' => StartMode}),
    %%==========log start begin========{rabbit_amqqueue_sup,24}==============
    %%#{'Q' =>
    %%      {amqqueue,{resource,<<"/">>,queue,<<"data.account_log">>},
    %%      true,false,none,[],<0.3314.0>,[],[],[],undefined,undefined,[],
    %%      undefined,live,0,[],<<"/">>,
    %%      #{user => <<"guest">>},
    %%      rabbit_classic_queue,#{}},
    %%'StartMode' => recovery}

    Marker = spawn_link(fun() -> receive stop -> ok end end),
    ChildSpec = {rabbit_amqqueue,
                 {rabbit_prequeue, start_link, [Q, StartMode, Marker]},
                 intrinsic, ?CLASSIC_QUEUE_WORKER_WAIT, worker,
                 [rabbit_amqqueue_process, rabbit_mirror_queue_slave]},
    {ok, SupPid} = supervisor2:start_link(?MODULE, []),
    {ok, QPid} = supervisor2:start_child(SupPid, ChildSpec),
    unlink(Marker),
    Marker ! stop,
    {ok, SupPid, QPid}.

init([]) -> {ok, {{one_for_one, 5, 10}, []}}.
