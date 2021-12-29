%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_channel_common).

-export([do/2, do/3, do_flow/3, ready_for_close/1]).

-include_lib("glib/include/log.hrl").

do(Pid, Method) ->
    do(Pid, Method, none).

do(Pid, Method, Content) ->
    ?LOG_CHANNEL_METHOD_CALL(#{'Method' => Method, 'Pid' => Pid, 'Content' => Content}),
    gen_server2:cast(Pid, {method, Method, Content, noflow}).

do_flow(Pid, Method, Content) ->
    R = glib_tool:pid_info(Pid),
    ?LOG1(#{mfa => R, 'Method' => Method, 'Content' => Content}),
    %% Here we are tracking messages sent by the rabbit_reader
    %% process. We are accessing the rabbit_reader process dictionary.
    credit_flow:send(Pid),
    gen_server2:cast(Pid, {method, Method, Content, flow}).

ready_for_close(Pid) ->
    gen_server2:cast(Pid, ready_for_close).
