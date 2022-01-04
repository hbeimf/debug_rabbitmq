%%%-------------------------------------------------------------------
%%% @author maomao
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 1月 2022 下午1:48
%%%-------------------------------------------------------------------
-module(demo_supervisor).
-author("maomao").

%%-behaviour(supervisor).
-behaviour(supervisor2).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor2:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

%%  AChild = #{id => 'AName',
%%    start => {'AModule', start_link, []},
%%    restart => permanent,
%%    shutdown => 2000,
%%    type => worker,
%%    modules => ['AModule']},

%%  {ok, {SupFlags, [AChild]}}.
  Children = children(),
  {ok, {SupFlags, Children}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
children() ->
  [
    child(demo_actor)
  ].

child(Mod) ->
  #{id => Mod,
    start => {Mod, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [Mod]}.