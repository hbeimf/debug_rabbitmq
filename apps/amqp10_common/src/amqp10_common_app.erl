%%%-------------------------------------------------------------------
%% @doc amqp10_common public API
%% @end
%%%-------------------------------------------------------------------

-module(amqp10_common_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    amqp10_common_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
