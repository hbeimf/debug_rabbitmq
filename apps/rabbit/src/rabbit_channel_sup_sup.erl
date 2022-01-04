%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_channel_sup_sup).

%% Supervisor for AMQP 0-9-1 channels. Every AMQP 0-9-1 connection has
%% one of these.
%%
%% See also rabbit_channel_sup, rabbit_connection_helper_sup, rabbit_reader.

-behaviour(supervisor2).

-export([start_link/0, start_channel/2]).

-export([init/1]).

-include_lib("rabbit_common/include/rabbit.hrl").

%%----------------------------------------------------------------------------
-include_lib("glib/include/log.hrl").

-spec start_link() -> rabbit_types:ok_pid_or_error().

%% 'connection.open' 刚建立ｔｃｐ连接就初始化一个 ｃｈａｎｎｅｌ　督程
%% 多个channel 将会复用同一个tcp连接
%% 一个连接上可以有 2047 个 channel, 写在 rabbit.app.src里的 channel_max 配置项里
start_link() ->
    supervisor2:start_link(?MODULE, []).

-spec start_channel(pid(), rabbit_channel_sup:start_link_args()) ->
          {'ok', pid(), {pid(), any()}}.

start_channel(Pid, Args) ->
%%    Mfa = glib_tool:pid_info(Pid),
%%    ?LOG_CREATE_CHANNEL(#{'Pid' => Pid, 'Args' => Args, 'Mfa' => Mfa}),
    supervisor2:start_child(Pid, [Args]).

%%----------------------------------------------------------------------------

init([]) ->
    ?LG_PROCESS_TYPE(channel_sup_sup),
    {ok, {{simple_one_for_one, 0, 1},
          [{channel_sup, {rabbit_channel_sup, start_link, []},
            temporary, infinity, supervisor, [rabbit_channel_sup]}]}}.
