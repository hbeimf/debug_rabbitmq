%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_mqtt_sup).
-behaviour(supervisor2).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("glib/include/log_mqtt.hrl").

-export([start_link/2, init/1, stop_listeners/0]).

-define(TCP_PROTOCOL, 'mqtt').
-define(TLS_PROTOCOL, 'mqtt/ssl').

start_link(Listeners, []) ->
    supervisor2:start_link({local, ?MODULE}, ?MODULE, [Listeners]).

init([{Listeners, SslListeners0}]) ->
    NumTcpAcceptors = application:get_env(rabbitmq_mqtt, num_tcp_acceptors, 10),
    ConcurrentConnsSups = application:get_env(rabbitmq_mqtt, num_conns_sups, 1),
    {ok, SocketOpts} = application:get_env(rabbitmq_mqtt, tcp_listen_options),
    {SslOpts, NumSslAcceptors, SslListeners}
        = case SslListeners0 of
              [] -> {none, 0, []};
              _  -> {rabbit_networking:ensure_ssl(),
                     application:get_env(rabbitmq_mqtt, num_ssl_acceptors, 10),
                     case rabbit_networking:poodle_check('MQTT') of
                         ok     -> SslListeners0;
                         danger -> []
                     end}
          end,
    Reply =
    {ok, {{one_for_all, 10, 10},
          [{rabbit_mqtt_retainer_sup,
            {rabbit_mqtt_retainer_sup, start_link, [{local, rabbit_mqtt_retainer_sup}]},
             transient, ?SUPERVISOR_WAIT, supervisor, [rabbit_mqtt_retainer_sup]} |
           listener_specs(fun tcp_listener_spec/1,
                          [SocketOpts, NumTcpAcceptors, ConcurrentConnsSups], Listeners) ++
           listener_specs(fun ssl_listener_spec/1,
                          [SocketOpts, SslOpts, NumSslAcceptors, ConcurrentConnsSups],
                          SslListeners)]}},
    % ?LOG(Reply, "start init"),
    Reply.

% "2022-01-10 15:28:59.927"========================================
% start init
% Ｍod: rabbit_mqtt_sup; Line: 46 ;
% {ok,{{one_for_all,10,10},
%      [{rabbit_mqtt_retainer_sup,
%           {rabbit_mqtt_retainer_sup,start_link,
%               [{local,rabbit_mqtt_retainer_sup}]},
%           transient,infinity,supervisor,
%           [rabbit_mqtt_retainer_sup]},
%       {'rabbit_mqtt_listener_sup_:::1883',
%           {tcp_listener_sup,start_link,
%               [{0,0,0,0,0,0,0,0},
%                1883,ranch_tcp,
%                [inet6,{backlog,128},{nodelay,true}],
%                rabbit_mqtt_connection_sup,[],
%                {rabbit_networking,tcp_listener_started,
%                    [mqtt,[{backlog,128},{nodelay,true}]]},
%                {rabbit_networking,tcp_listener_stopped,
%                    [mqtt,[{backlog,128},{nodelay,true}]]},
%                10,1,"MQTT TCP listener"]},
%           transient,infinity,supervisor,
%           [tcp_listener_sup]}]}}

stop_listeners() ->
    rabbit_networking:stop_ranch_listener_of_protocol(?TCP_PROTOCOL),
    rabbit_networking:stop_ranch_listener_of_protocol(?TLS_PROTOCOL),
    ok.

%%
%% Implementation
%%

listener_specs(Fun, Args, Listeners) ->
    [Fun([Address | Args]) ||
        Listener <- Listeners,
        Address  <- rabbit_networking:tcp_listener_addresses(Listener)].

tcp_listener_spec([Address, SocketOpts, NumAcceptors, ConcurrentConnsSups]) ->
    rabbit_networking:tcp_listener_spec(
      rabbit_mqtt_listener_sup, Address, SocketOpts,
      transport(?TCP_PROTOCOL), rabbit_mqtt_connection_sup, [],
      mqtt, NumAcceptors, ConcurrentConnsSups, "MQTT TCP listener").

ssl_listener_spec([Address, SocketOpts, SslOpts, NumAcceptors, ConcurrentConnsSups]) ->
    rabbit_networking:tcp_listener_spec(
      rabbit_mqtt_listener_sup, Address, SocketOpts ++ SslOpts,
      transport(?TLS_PROTOCOL), rabbit_mqtt_connection_sup, [],
      'mqtt/ssl', NumAcceptors, ConcurrentConnsSups, "MQTT TLS listener").

transport(Protocol) ->
    case Protocol of
        ?TCP_PROTOCOL -> ranch_tcp;
        ?TLS_PROTOCOL -> ranch_ssl
    end.
