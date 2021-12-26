-module(rabbit_prelaunch_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include_lib("glib/include/log.hrl").

start(_Type, _Args) ->
    % ?LOG(here0),
    rabbit_prelaunch_sup:start_link().

stop(_State) ->
    ok.
