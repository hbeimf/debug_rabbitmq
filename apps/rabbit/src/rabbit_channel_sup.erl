%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_channel_sup).

%% Supervises processes that implement AMQP 0-9-1 channels:
%%
%%  * Channel process itself
%%  * Network writer (for network connections)
%%  * Limiter (handles channel QoS and flow control)
%%
%% Every rabbit_channel_sup is supervised by rabbit_channel_sup_sup.
%%
%% See also rabbit_channel, rabbit_writer, rabbit_limiter.

-behaviour(supervisor2).

-export([start_link/1]).

-export([init/1]).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("glib/include/log.hrl").
%%----------------------------------------------------------------------------

-export_type([start_link_args/0]).

-type start_link_args() ::
        {'tcp', rabbit_net:socket(), rabbit_channel:channel_number(),
         non_neg_integer(), pid(), string(), rabbit_types:protocol(),
         rabbit_types:user(), rabbit_types:vhost(), rabbit_framing:amqp_table(),
         pid()} |
        {'direct', rabbit_channel:channel_number(), pid(), string(),
         rabbit_types:protocol(), rabbit_types:user(), rabbit_types:vhost(),
         rabbit_framing:amqp_table(), pid()}.

-define(FAIR_WAIT, 70000).

%%----------------------------------------------------------------------------

-spec start_link(start_link_args()) -> {'ok', pid(), {pid(), any()}}.

%% ReaderPid 是 rabbit_reader
start_link({tcp, Sock, Channel, FrameMax, ReaderPid, ConnName, Protocol, User,
            VHost, Capabilities, Collector}) ->
    %% ?LOG_CREATE_CHANNEL(#{'Args' => {tcp, Sock, Channel, FrameMax, ReaderPid, ConnName, Protocol, User, VHost, Capabilities, Collector}}),

    %% 下面一次启动了两人个 actor
    %% rabbit_writer:start_link &&  rabbit_limiter:start_link
    {ok, SupPid} = supervisor2:start_link(
                     ?MODULE, {tcp, Sock, Channel, FrameMax,
                               ReaderPid, Protocol, {ConnName, Channel}}),

    %% 取出上面刚刚创建的两人个子进程的 ｐｉｄ
    [LimiterPid] = supervisor2:find_child(SupPid, limiter),
    [WriterPid] = supervisor2:find_child(SupPid, writer),
    %% ?LOG_CREATE_CHANNEL(#{'LimiterPid' => LimiterPid, 'mfa' => glib_tool:pid_info(LimiterPid), 'WriterPid' => WriterPid, 'mfa1' => glib_tool:pid_info(WriterPid)}),

    {ok, ChannelPid} =
        supervisor2:start_child(
          SupPid,
          {channel, {rabbit_channel, start_link,
                     [Channel, ReaderPid, WriterPid, ReaderPid, ConnName,
                      Protocol, User, VHost, Capabilities, Collector,
                      LimiterPid]},
           intrinsic, ?FAIR_WAIT, worker, [rabbit_channel]}),
           
    {ok, AState} = rabbit_command_assembler:init(Protocol),
    {ok, SupPid, {ChannelPid, AState}};
start_link({direct, Channel, ClientChannelPid, ConnPid, ConnName, Protocol,
            User, VHost, Capabilities, Collector, AmqpParams}) ->
    {ok, SupPid} = supervisor2:start_link(
                     ?MODULE, {direct, {ConnName, Channel}}),
    [LimiterPid] = supervisor2:find_child(SupPid, limiter),
    {ok, ChannelPid} =
        supervisor2:start_child(
          SupPid,
          {channel, {rabbit_channel, start_link,
                     [Channel, ClientChannelPid, ClientChannelPid, ConnPid,
                      ConnName, Protocol, User, VHost, Capabilities, Collector,
                      LimiterPid, AmqpParams]},
           intrinsic, ?FAIR_WAIT, worker, [rabbit_channel]}),
    {ok, SupPid, {ChannelPid, none}}.

%%----------------------------------------------------------------------------

init(Type) ->
    ?LG_PROCESS_TYPE(channel_sup),
    Child = child_specs(Type),
    ?LOG_CREATE_CHANNEL(#{'Child' => Child}),
    {ok, {{one_for_all, 0, 1}, Child}}.

% ==========log create channel ========{rabbit_channel_sup,90}==============
% #{'Child' =>
%         [{writer,{rabbit_writer,start_link,
%                                 [#Port<0.496>,1,131072,
%                                 rabbit_framing_amqp_0_9_1,<0.3227.0>,
%                                 {<<"127.0.0.1:52585 -> 127.0.0.1:5672">>,1},
%                                 true]},
%                 intrinsic,70000,worker,
%                 [rabbit_writer]},
%         {limiter,{rabbit_limiter,start_link,
%                                 [{<<"127.0.0.1:52585 -> 127.0.0.1:5672">>,1}]},
%                 transient,70000,worker,
%                 [rabbit_limiter]}]}
    

child_specs({tcp, Sock, Channel, FrameMax, ReaderPid, Protocol, Identity}) ->
    [{writer, {rabbit_writer, start_link,
               [Sock, Channel, FrameMax, Protocol, ReaderPid, Identity, true]},
      intrinsic, ?FAIR_WAIT, worker, [rabbit_writer]}
     | child_specs({direct, Identity})];
child_specs({direct, Identity}) ->
    [{limiter, {rabbit_limiter, start_link, [Identity]},
      transient, ?FAIR_WAIT, worker, [rabbit_limiter]}].
