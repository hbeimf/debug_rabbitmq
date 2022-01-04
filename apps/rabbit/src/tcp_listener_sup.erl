%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(tcp_listener_sup).

%% Supervises TCP listeners. There is a separate supervisor for every
%% protocol. In case of AMQP 0-9-1, it resides under rabbit_sup. Plugins
%% that provide protocol support (e.g. STOMP) have an instance of this supervisor in their
%% app supervision tree.
%%
%% See also rabbit_networking and tcp_listener.

-behaviour(supervisor).

-export([start_link/11]).
-export([init/1]).

-include_lib("glib/include/log.hrl").

-type mfargs() :: {atom(), atom(), [any()]}.

-spec start_link
        (inet:ip_address(), inet:port_number(), module(), [gen_tcp:listen_option()],
         module(), any(), mfargs(), mfargs(), integer(), integer(), string()) ->
                           rabbit_types:ok_pid_or_error().

%% 启动网络端口
start_link(IPAddress, Port, Transport, SocketOpts, ProtoSup, ProtoOpts, OnStartup, OnShutdown,
           ConcurrentAcceptorCount, ConcurrentConnsSups, Label) ->
    supervisor:start_link(
      ?MODULE, {IPAddress, Port, Transport, SocketOpts, ProtoSup, ProtoOpts, OnStartup, OnShutdown,
                ConcurrentAcceptorCount, ConcurrentConnsSups, Label}).

%%==========log begin========{tcp_listener_sup,40}==============
%%{{0,0,0,0,0,0,0,0},
%%5672,ranch_tcp,
%%[inet6,{backlog,128},{nodelay,true},{linger,{true,0}},{exit_on_close,false}],
%%rabbit_connection_sup,[],
%%{rabbit_networking,tcp_listener_started,
%%        [amqp,
%%        [{backlog,128},
%%        {nodelay,true},
%%        {linger,{true,0}},
%%        {exit_on_close,false}]]},
%%{rabbit_networking,tcp_listener_stopped,
%%      [amqp,
%%      [{backlog,128},
%%      {nodelay,true},
%%      {linger,{true,0}},
%%      {exit_on_close,false}]]},
%%10,1,"TCP listener"}

init({IPAddress, Port, Transport, SocketOpts, ProtoSup, ProtoOpts, OnStartup, OnShutdown,
      ConcurrentAcceptorCount, ConcurrentConnsSups, Label}) ->

%%    ?LOG({IPAddress, Port, Transport, SocketOpts, ProtoSup, ProtoOpts, OnStartup, OnShutdown,
%%    ConcurrentAcceptorCount, ConcurrentConnsSups, Label}),

    {ok, AckTimeout} = application:get_env(rabbit, ssl_handshake_timeout),
    MaxConnections = max_conn(rabbit_misc:get_env(rabbit, connection_max, infinity),
                              ConcurrentConnsSups),
    RanchListenerOpts = #{
      num_acceptors => ConcurrentAcceptorCount,
      max_connections => MaxConnections,
      handshake_timeout => AckTimeout,
      connection_type => supervisor,
      socket_opts => [{ip, IPAddress},
                      {port, Port} |
                      SocketOpts],
      num_conns_sups => ConcurrentConnsSups
     },
    Flags = {one_for_all, 10, 10},
    OurChildSpecStart = {tcp_listener, start_link, [IPAddress, Port, OnStartup, OnShutdown, Label]},
    OurChildSpec = {tcp_listener, OurChildSpecStart, transient, 16#ffffffff, worker, [tcp_listener]},
    RanchChildSpec = ranch:child_spec(rabbit_networking:ranch_ref(IPAddress, Port),
        Transport, RanchListenerOpts,
        ProtoSup, ProtoOpts),

%%    ?LOG(#{one => RanchChildSpec, two => OurChildSpec, pid => self()}),
    {ok, {Flags, [RanchChildSpec, OurChildSpec]}}.

max_conn(infinity, _) ->
    infinity;
max_conn(Max, Sups) ->
  %% connection_max in Ranch is per connection supervisor
  Max div Sups.

%%==========log begin========{tcp_listener_sup,82}==============

%%这个ａｃｔｏｒ　才是启动网络的主线,　这里启动的方式比往常的启动更低阶一点,
%%握手的时候将　ｓｏｃｋｅｔ　句柄传递给　ｒａｂｂｉｔ_ｒｅａｄｅｒ actor
%%在那个　ａｃｔｏｒ　里进行网络数据的读写操作,　那里是业务逻辑的起点
%% ranch里的建立连接相关的快速过一下吧,最后会转到 rabbit_connection_sup:start_link/3
%% ｒａｂｂｉｔ_ｒｅａｄｅｒ:ｉｎｉｔ里通过握手拿到　ｓｏｃｋｅｔ后,至此ｔｃｐ连接 的建立差不多就接近尾声了,
%%建议将ｒａｎｃｈ单独拿出来撸, 一个非常优秀的包, 谁用谁知道

%%#{one =>
%%    #{id => {ranch_embedded_sup,{acceptor,{0,0,0,0,0,0,0,0},5672}},
%%    start =>
%%        {ranch_embedded_sup,start_link,
%%        [{acceptor,{0,0,0,0,0,0,0,0},5672},
%%        ranch_tcp,
%%            #{connection_type => supervisor,handshake_timeout => 5000,
%%                  max_connections => infinity,num_acceptors => 10,
%%                  num_conns_sups => 1,
%%                  socket_opts =>
%%                  [{ip,{0,0,0,0,0,0,0,0}},
%%                  {port,5672},
%%                  inet6,
%%                  {backlog,128},
%%                  {nodelay,true},
%%                  {linger,{true,0}},
%%                  {exit_on_close,false}]},
%%            rabbit_connection_sup,[]]},
%%    type => supervisor},
%%    pid => <0.4857.0>,


%%下面这个　ａｃｔｏｒ　主要是在分布式表
%%　rabbit_listener
%%里写一条记录,　记录下监听端口的一些属性,　实为跑龙套业务,　
%%主要的网络业务还是上一个ａｃｔｏｒ里,　

%%two =>
%%    {tcp_listener,
%%          {tcp_listener,start_link,
%%          [{0,0,0,0,0,0,0,0},
%%          5672,
%%          {rabbit_networking,tcp_listener_started,
%%              [amqp,
%%              [{backlog,128},
%%              {nodelay,true},
%%              {linger,{true,0}},
%%              {exit_on_close,false}]]},
%%          {rabbit_networking,tcp_listener_stopped,
%%                [amqp,
%%                [{backlog,128},
%%                {nodelay,true},
%%                {linger,{true,0}},
%%                {exit_on_close,false}]]},
%%          "TCP listener"]},
%%    transient,4294967295,worker,
%%    [tcp_listener]}}
