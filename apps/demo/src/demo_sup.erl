%%%-------------------------------------------------------------------
%% @doc demo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(demo_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
%%init([]) ->
%%    SupFlags = #{strategy => one_for_all,
%%                 intensity => 0,
%%                 period => 1},
%%    ChildSpecs = [],
%%    {ok, {SupFlags, ChildSpecs}}.
%%
%%%% internal functions

init([]) ->
  Children = children(),

  {ok, { {one_for_one, 10, 10}, Children} }.
%%====================================================================
%% Internal functions
%%====================================================================
children() ->
  [
    child_sup(demo_supervisor)
    % , child(glib_cluster_actor)
    % , child(glib_memory_actor)
  ].


%%child(Mod) ->
%%  Child = {Mod, {Mod, start_link, []},
%%    permanent, 5000, worker, [Mod]},
%%  Child.

child_sup(Mod) ->
  Child = {Mod, {Mod, start_link, []},
    permanent, 5000, supervisor, [Mod]},
  Child.