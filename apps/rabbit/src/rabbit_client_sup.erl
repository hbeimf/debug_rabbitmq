%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_client_sup).

-behaviour(supervisor2).

-export([start_link/1, start_link/2, start_link_worker/2]).

-export([init/1]).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("glib/include/log.hrl").
%%----------------------------------------------------------------------------

-spec start_link(rabbit_types:mfargs()) ->
          rabbit_types:ok_pid_or_error().

start_link(Callback) ->
%%    ?LOG_START(Callback),
    supervisor2:start_link(?MODULE, Callback).

-spec start_link({'local', atom()}, rabbit_types:mfargs()) ->
          rabbit_types:ok_pid_or_error().

start_link(SupName, Callback) ->
%%  ?LOG_START({SupName, Callback}),
    supervisor2:start_link(SupName, ?MODULE, Callback).

%%==========log start begin========{rabbit_client_sup,31}==============
%%{rabbit_channel_sup,start_link,[]}
%%==========log start begin========{rabbit_client_sup,31}==============
%%{{local,rabbit_direct_client_sup},{rabbit_channel_sup,start_link,[]}}

-spec start_link_worker({'local', atom()}, rabbit_types:mfargs()) ->
          rabbit_types:ok_pid_or_error().

start_link_worker(SupName, Callback) ->
%%    ?LOG_START({SupName, Callback}),
    supervisor2:start_link(SupName, ?MODULE, {Callback, worker}).

init({M,F,A}) ->
%%    ?LOG_START({M,F,A}),
    {ok, {{simple_one_for_one, 0, 1},
          [{client, {M,F,A}, temporary, infinity, supervisor, [M]}]}};
init({{M,F,A}, worker}) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{client, {M,F,A}, temporary, ?WORKER_WAIT, worker, [M]}]}}.

%%
%%==========log start begin========{rabbit_client_sup,47}==============
%%{rabbit_channel_sup,start_link,[]}
