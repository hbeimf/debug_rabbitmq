%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_boot_steps).

-export([run_boot_steps/0, run_boot_steps/1, run_cleanup_steps/1]).
-export([find_steps/0, find_steps/1]).
-include_lib("glib/include/log.hrl").
-include_lib("sys_log/include/write_log.hrl").

run_boot_steps() ->
    run_boot_steps(loaded_applications()).

%% 此处由 rabbit 启动函数调用
%%启动一些在网络启动之前就要启动的业务,
run_boot_steps(Apps) ->
    %%    ?LOG_START(Apps),
    %%  ==========log start begin========{rabbit_boot_steps,21}==============
    %%[rabbit]

    [begin
      rabbit_log:info("Running boot step ~s defined by app ~s", [Step, App]),
    %    ?LOG_START({App, Step, Attrs}), %% [1]
      ok = run_step(Attrs, mfa)
    end || {App, Step, Attrs} <- find_steps(Apps)],
    ok.

run_cleanup_steps(Apps) ->
    [run_step(Attrs, cleanup) || {_, _, Attrs} <- find_steps(Apps)],
    ok.

loaded_applications() ->
    [App || {App, _, _} <- application:loaded_applications()].

find_steps() ->
    find_steps(loaded_applications()).

find_steps(Apps) ->
    All = sort_boot_steps(rabbit_misc:all_module_attributes(rabbit_boot_step)),
    [Step || {App, _, _} = Step <- All, lists:member(App, Apps)].

run_step(Attributes, AttributeName) ->
    [begin
        rabbit_log:debug("Applying MFA: M = ~s, F = ~s, A = ~p",
                        [M, F, A]),

        % ?LOG2({M, F, A}),
        % Log = lists:concat([glib:to_str(M), "__", glib:to_str(F)]),
        % ?WRITE_LOG(Log, #{m => M, f => F, a => A}),
        % ?LOG_START({M, F, A}), %% [2]
        case apply(M,F,A) of
            ok              ->
                rabbit_log:debug("Finished MFA: M = ~s, F = ~s, A = ~p",
                                 [M, F, A]);
            {error, Reason} -> exit({error, Reason})
        end
     end
      || {Key, {M,F,A}} <- Attributes,
          Key =:= AttributeName],
    ok.

vertices({AppName, _Module, Steps}) ->
    [{StepName, {AppName, StepName, Atts}} || {StepName, Atts} <- Steps].

edges({_AppName, _Module, Steps}) ->
    EnsureList = fun (L) when is_list(L) -> L;
                     (T)                 -> [T]
                 end,
    [case Key of
         requires -> {StepName, OtherStep};
         enables  -> {OtherStep, StepName}
     end || {StepName, Atts} <- Steps,
            {Key, OtherStepOrSteps} <- Atts,
            OtherStep <- EnsureList(OtherStepOrSteps),
            Key =:= requires orelse Key =:= enables].

sort_boot_steps(UnsortedSteps) ->
    case rabbit_misc:build_acyclic_graph(fun vertices/1, fun edges/1,
                                         UnsortedSteps) of
        {ok, G} ->
            %% Use topological sort to find a consistent ordering (if
            %% there is one, otherwise fail).
            SortedSteps = lists:reverse(
                            [begin
                                 {StepName, Step} = digraph:vertex(G,
                                                                   StepName),
                                 Step
                             end || StepName <- digraph_utils:topsort(G)]),
            digraph:delete(G),
            %% Check that all mentioned {M,F,A} triples are exported.
            case [{StepName, {M,F,A}} ||
                     {_App, StepName, Attributes} <- SortedSteps,
                     {mfa, {M,F,A}}               <- Attributes,
                     code:ensure_loaded(M) =/= {module, M} orelse
                     not erlang:function_exported(M, F, length(A))] of
                []         -> SortedSteps;
                MissingFns -> exit({boot_functions_not_exported, MissingFns})
            end;
        {error, {vertex, duplicate, StepName}} ->
            exit({duplicate_boot_step, StepName});
        {error, {edge, Reason, From, To}} ->
            exit({invalid_boot_step_dependency, From, To, Reason})
    end.


% 网络启动前启动的一些业务
% [2]
% Starting broker...
% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_global_counters,boot_step,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_child,[rabbit_osiris_metrics]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_child,[rabbit_metrics]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_alarm,start,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_child,[code_server_cache]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit,start_fhc,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_supervisor_child,[worker_pool_sup]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_mnesia,init,[]}
% 初始化分布式表相关的业务. 相关模块: rabbit_mnesia; rabbit_table


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_child,[mnesia_sync]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_feature_flags,init,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_binary_generator,check_empty_frame_size,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_child,[rabbit_registry]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [auth_mechanism,<<"RABBIT-CR-DEMO">>,
%                   rabbit_auth_mechanism_cr_demo]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [queue_master_locator,<<"random">>,
%                   rabbit_queue_location_random]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_restartable_child,[rabbit_event]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [auth_mechanism,<<"AMQPLAIN">>,
%                   rabbit_auth_mechanism_amqplain]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [auth_mechanism,<<"PLAIN">>,rabbit_auth_mechanism_plain]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,[exchange,<<"direct">>,rabbit_exchange_type_direct]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,[exchange,<<"fanout">>,rabbit_exchange_type_fanout]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [exchange,<<"headers">>,rabbit_exchange_type_headers]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,[exchange,<<"topic">>,rabbit_exchange_type_topic]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,[ha_mode,<<"all">>,rabbit_mirror_queue_mode_all]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [ha_mode,<<"exactly">>,rabbit_mirror_queue_mode_exactly]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [ha_mode,<<"nodes">>,rabbit_mirror_queue_mode_nodes]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_priority_queue,enable,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [queue_master_locator,<<"client-local">>,
%                   rabbit_queue_location_client_local]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [queue_master_locator,<<"min-masters">>,
%                   rabbit_queue_location_min_masters]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_restartable_child,[rabbit_sysmon_minder]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_restartable_child,[rabbit_epmd_monitor]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_restartable_child,[rabbit_guid]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_restartable_child,[rabbit_node_monitor]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit,boot_delegate,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_restartable_child,[rabbit_memory_monitor]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_upgrade,maybe_migrate_queues_to_per_vhost_storage,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_channel_tracking,boot,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {gen_event,add_handler,[rabbit_event,rabbit_channel_tracking_handler,[]]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_connection_tracking,boot,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {gen_event,add_handler,[rabbit_event,rabbit_connection_tracking_handler,[]]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_exchange_parameters,register,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [policy_validator,<<"ha-mode">>,rabbit_mirror_queue_misc]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [policy_validator,<<"ha-params">>,rabbit_mirror_queue_misc]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [policy_validator,<<"ha-sync-mode">>,
%                   rabbit_mirror_queue_misc]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [policy_validator,<<"ha-sync-batch-size">>,
%                   rabbit_mirror_queue_misc]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [policy_validator,<<"ha-promote-on-shutdown">>,
%                   rabbit_mirror_queue_misc]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [policy_validator,<<"ha-promote-on-failure">>,
%                   rabbit_mirror_queue_misc]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_policies,register,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_policy,register,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_registry,register,
%                  [policy_validator,<<"queue-master-locator">>,
%                   rabbit_queue_location_validator]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_quorum_memory_manager,register,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_stream_coordinator,recover,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_vhost_limit,register,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit,recover,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit,maybe_insert_default_data,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_looking_glass,boot,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_restartable_child,[rabbit_core_metrics_gc]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_sup,start_restartable_child,[background_gc]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_node_monitor,notify_node_up,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {logger,debug,
%         ["'networking' boot step skipped and moved to end of startup",[],
%          #{domain => [rabbitmq]}]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_definitions,boot,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_nodes,boot,[]}


% ==========log start begin========{rabbit_boot_steps,54}==============
% {rabbit_direct,boot,[]}




