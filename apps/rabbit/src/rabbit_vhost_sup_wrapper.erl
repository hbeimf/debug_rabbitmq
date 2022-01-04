%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2017-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

%% This module is a wrapper around vhost supervisor to
%% provide exactly once restart semantics.

-module(rabbit_vhost_sup_wrapper).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("glib/include/log.hrl").

-behaviour(supervisor2).
-export([init/1]).
-export([start_link/1]).
-export([start_vhost_sup/1]).

start_link(VHost) ->
%%  ?LOG_START({VHost, self()}),
    %% Using supervisor, because supervisor2 does not stop a started child when
    %% another one fails to start. Bug?
    case rabbit_vhost_sup_sup:get_vhost_sup(VHost) of
        {ok, Pid}  ->
%%          ?LOG_START(Pid),
            {error, {already_started, Pid}};
        {error, _} ->
%%          ?LOG_START(VHost),
            supervisor:start_link(?MODULE, [VHost])
    end.

%%==========log start begin========{rabbit_vhost_sup_wrapper,22}==============
%%<<"/">>


init([VHost]) ->
%%  ?LOG_START(VHost),
    %% 2 restarts in 5 minutes. One per message store.
    {ok, {{one_for_all, 2, 300},
        [
        %% rabbit_vhost_sup is an empty supervisor container for
        %% all data processes.
         {rabbit_vhost_sup,
          {rabbit_vhost_sup_wrapper, start_vhost_sup, [VHost]},
           permanent, infinity, supervisor,
           [rabbit_vhost_sup]},
        %% rabbit_vhost_process is a vhost identity process, which
        %% is responsible for data recovery and vhost aliveness status.
        %% See the module comments for more info.
          %% 启动时从这里跟下去,可以一直跟到启动已经存在的队列的初始化,
         {rabbit_vhost_process,
          {rabbit_vhost_process, start_link, [VHost]},
           permanent, ?WORKER_WAIT, worker,
           [rabbit_vhost_process]}]}}.


start_vhost_sup(VHost) ->
%%  ?LOG_START(VHost),
     case rabbit_vhost_sup:start_link(VHost) of
        {ok, Pid} ->
            %% Save vhost sup record with wrapper pid and vhost sup pid.
            ok = rabbit_vhost_sup_sup:save_vhost_sup(VHost, self(), Pid),
            {ok, Pid};
        Other ->
            Other
    end.
%%
%%==========log start begin========{rabbit_vhost_sup_wrapper,59}==============
%%<<"/">>
