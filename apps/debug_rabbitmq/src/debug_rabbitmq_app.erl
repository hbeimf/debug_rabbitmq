%%%-------------------------------------------------------------------
%% @doc debug_rabbitmq public API
%% @end
%%%-------------------------------------------------------------------

-module(debug_rabbitmq_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rabbit:start(),
    % rabbit:boot(),
    
    debug_rabbitmq_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
