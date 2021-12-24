%%%-------------------------------------------------------------------
%% @doc rabbit public API
%% @end
%%%-------------------------------------------------------------------

-module(rabbit_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rabbit_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
