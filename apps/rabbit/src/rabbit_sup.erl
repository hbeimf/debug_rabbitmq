%%%-------------------------------------------------------------------
%% @doc rabbit top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rabbit_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	% rabbit:receive_demo(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
       % Send = {rabbit_log_send, {rabbit_log_send, start_link, []},
       %         permanent, 5000, worker, [rabbit_log_send]},
               
    Children = [
    	% child(rabbit_log_send)
    	% , child(rabbit_error_log_send)
        child(rabbit_pub_account_log)
        % , child(rabbit_pub_game_log)
        % , child(pub_cache)
        % , child(rabbit_pub_channel_credits_log)
        % , child(rabbit_pub_modify_account_interface)
    ],

    {ok, { {one_for_one, 10, 10}, Children} }.


%%====================================================================
%% Internal functions
%%====================================================================
child(Mod) ->
	Child = {Mod, {Mod, start_link, []},
               permanent, 5000, worker, [Mod]},
               Child.


