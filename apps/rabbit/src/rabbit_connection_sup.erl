%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_connection_sup).

%% Supervisor for a (network) AMQP 0-9-1 client connection.
%%
%% Supervises
%%
%%  * rabbit_reader
%%  * Auxiliary process supervisor
%%
%% See also rabbit_reader, rabbit_connection_helper_sup.

-behaviour(supervisor2).
-behaviour(ranch_protocol).

-export([start_link/3, reader/1]).

-export([init/1]).

-include_lib("rabbit_common/include/rabbit.hrl").

%%----------------------------------------------------------------------------

-spec start_link(any(), module(), any()) ->
          {'ok', pid(), pid()}.

%% 这里由 tcp_listener_sup 模块穿越ranch包启动,

start_link(Ref, _Transport, _Opts) ->
    {ok, SupPid} = supervisor2:start_link(?MODULE, []),
    %% We need to get channels in the hierarchy here so they get shut
    %% down after the reader, so the reader gets a chance to terminate
    %% them cleanly. But for 1.0 readers we can't start the real
    %% ch_sup_sup (because we don't know if we will be 0-9-1 or 1.0) -
    %% so we add another supervisor into the hierarchy.
    %%
    %% This supervisor also acts as an intermediary for heartbeaters and
    %% the queue collector process, since these must not be siblings of the
    %% reader due to the potential for deadlock if they are added/restarted
    %% whilst the supervision tree is shutting down.

    %%　下面这个目前是个空督程, 后面会在这个 sup 下面启动一个 rabbit_channel_sup_sup的 sup1, 并且会在 sup1的下面启动一系列 actor,
    %%　其中一个重要的 actor : rabbit_channel  就是包括在这个里面,

    {ok, HelperSup} =
        supervisor2:start_child(
          SupPid,
          {helper_sup, {rabbit_connection_helper_sup, start_link, []},
           intrinsic, infinity, supervisor, [rabbit_connection_helper_sup]}),

    %% 下面这个actor 会负责从tcp连接里读写数据 ,解包啥的,通信部分相关的细节都由下面这个actor来处理.
    %%  然后将处理好的数据发送给 rabbit_channel 来处理相关的具体逻辑 ,
    {ok, ReaderPid} =
        supervisor2:start_child(
          SupPid,
          {reader, {rabbit_reader, start_link, [HelperSup, Ref]},
           intrinsic, ?WORKER_WAIT, worker, [rabbit_reader]}),
    {ok, SupPid, ReaderPid}.

-spec reader(pid()) -> pid().

reader(Pid) ->
    hd(supervisor2:find_child(Pid, reader)).

%%--------------------------------------------------------------------------

init([]) ->
    ?LG_PROCESS_TYPE(connection_sup),
    {ok, {{one_for_all, 0, 1}, []}}.
