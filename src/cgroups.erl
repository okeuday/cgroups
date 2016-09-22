%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==cgroups Manipulation Functions==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2016, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2016 Michael Truog
%%% @version 1.5.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cgroups).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([create/4,
         destroy/1,
         new/0,
         new/1,
         shell/2,
         update/4,
         update_or_create/4]).

-record(cgroups,
    {
        version :: pos_integer(),
        path :: string(),
        mounted :: boolean()
    }).
-define(APPLICATION, cgroups).

-type options() :: list({version_default, pos_integer()} |
                        {version_default_required, boolean()} |
                        {path_v1, string()} |
                        {path_v2, string()} |
                        {path_mounts, string() | undefined}).
-export_type([options/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===.===
%% @end
%%-------------------------------------------------------------------------

-spec create(CGroupPath :: string(),
             OSPids :: list(pos_integer()),
             CGroupParameters :: list({string(), string()}),
             State :: #cgroups{}) ->
    {ok, #cgroups{}} |
    {error, any()}.

create(CGroupPath, OSPids, CGroupParameters,
       #cgroups{path = Path} = State) ->
    true = cgroup_path_valid(CGroupPath),
    CGroupPathFull = Path ++ CGroupPath,
    case filelib:is_dir(CGroupPathFull) of
        true ->
            {error, {exists, CGroupPathFull}};
        false ->
            case shell("mkdir -p \"~s\"", [CGroupPathFull]) of
                {0, _} ->
                    update(CGroupPath, OSPids, CGroupParameters, State);
                {Status, Output} ->
                    {error, {mkdir, Status, Output}}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Destroy cgroups state data.===
%% @end
%%-------------------------------------------------------------------------

-spec destroy(#cgroups{}) ->
    ok.

destroy(#cgroups{mounted = false}) ->
    ok;
destroy(#cgroups{path = Path,
                 mounted = true}) ->
    _ = shell("umount \"~s\"", [Path]),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create new cgroups state data.===
%% @end
%%-------------------------------------------------------------------------

-spec new() ->
    {ok, #cgroups{}} |
    {error, any()}.

new() ->
    new([]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create new cgroups state data with local options.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Options0 :: options()) ->
    {ok, #cgroups{}} |
    {error, any()}.

new(Options0) ->
    {VersionDefault,
     Options1} = option(version_default, Options0),
    {VersionDefaultRequired,
     Options2} = option(version_default_required, Options1),
    {PathV1,
     Options3} = option(path_v1, Options2),
    {PathV2,
     Options4} = option(path_v2, Options3),
    {PathMounts,
     OptionsN} = option(path_mounts, Options4),
    [] = OptionsN,
    true = is_integer(VersionDefault) andalso (VersionDefault > 0),
    true = is_boolean(VersionDefaultRequired),
    true = is_list(PathV1) andalso
           ($/ == hd(lists:reverse(PathV1))) andalso (length(PathV1) > 1),
    true = is_list(PathV2) andalso
           ($/ == hd(lists:reverse(PathV2))) andalso (length(PathV2) > 1),
    true = (PathMounts =:= undefined) orelse
           (is_list(PathMounts) andalso is_integer(hd(PathMounts))),
    new_state(VersionDefault, VersionDefaultRequired,
              PathV1, PathV2, PathMounts).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute a command with the default shell.===
%% @end
%%-------------------------------------------------------------------------

-spec shell(Command :: string(),
            Arguments :: list()) ->
    {non_neg_integer(), list(binary())}.

shell(Command, Arguments) ->
    Shell = erlang:open_port({spawn_executable, "/bin/sh"},
                             [{args, ["-"]}, {cd, "/"},
                              stream, binary, stderr_to_stdout, exit_status]),
    Exec = io_lib:format(Command, Arguments),
    true = erlang:port_command(Shell, ["exec ", Exec, "\n"]),
    shell_output(Shell, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===.===
%% @end
%%-------------------------------------------------------------------------

-spec update(CGroupPath :: string(),
             OSPids :: list(pos_integer()),
             CGroupParameters :: list({string(), string()}),
             State :: #cgroups{}) ->
    {ok, #cgroups{}} |
    {error, any()}.

update(CGroupPath, OSPids, CGroupParameters,
       #cgroups{version = Version,
                path = Path} = State) ->
    true = cgroup_path_valid(CGroupPath),
    CGroupPathFull = Path ++ CGroupPath,
    case update_parameters(CGroupParameters, Version, CGroupPathFull) of
        ok ->
            case update_pids(OSPids, Version, CGroupPathFull) of
                ok ->
                    {ok, State};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===.===
%% @end
%%-------------------------------------------------------------------------

-spec update_or_create(CGroupPath :: string(),
                       OSPids :: list(pos_integer()),
                       CGroupParameters :: list({string(), string()}),
                       State :: #cgroups{}) ->
    {ok, #cgroups{}} |
    {error, any()}.

update_or_create(CGroupPath, OSPids, CGroupParameters,
                 #cgroups{path = Path} = State) ->
    true = cgroup_path_valid(CGroupPath),
    CGroupPathFull = Path ++ CGroupPath,
    case filelib:is_dir(CGroupPathFull) of
        true ->
            update(CGroupPath, OSPids, CGroupParameters, State);
        false ->
            case shell("mkdir -p \"~s\"", [CGroupPathFull]) of
                {0, _} ->
                    update(CGroupPath, OSPids, CGroupParameters, State);
                {Status, Output} ->
                    {error, {mkdir, Status, Output}}
            end
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

new_state(1 = Version, VersionDefaultRequired,
          PathV1, PathV2, PathMounts) ->
    case new_paths(PathV1, PathV2, PathMounts) of
        {ok, {MountedV1, NewPathV1}, {MountedV2, NewPathV2}} ->
            case new_state_init(Version, MountedV1, NewPathV1) of
                {ok, _} = Success ->
                    Success;
                {error, _} = Error when VersionDefaultRequired =:= false ->
                    case new_state_init(2, MountedV2, NewPathV2) of
                        {ok, _} = Success ->
                            Success;
                        {error, _} ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
new_state(2 = Version, VersionDefaultRequired,
          PathV1, PathV2, PathMounts) ->
    case new_paths(PathV1, PathV2, PathMounts) of
        {ok, {MountedV1, NewPathV1}, {MountedV2, NewPathV2}} ->
            case new_state_init(Version, MountedV2, NewPathV2) of
                {ok, _} = Success ->
                    Success;
                {error, _} = Error when VersionDefaultRequired =:= false ->
                    case new_state_init(1, MountedV1, NewPathV1) of
                        {ok, _} = Success ->
                            Success;
                        {error, _} ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
new_state(Version, _, _, _, _) ->
    {error, {version_default, Version}}.

new_state_init(Version, true, Path) ->
    {ok, #cgroups{version = Version,
                  path = Path,
                  mounted = false}};
new_state_init(1 = Version, false, Path) ->
    case shell("mount -t cgroup none \"~s\"", [Path]) of
        {0, _} ->
            {ok, #cgroups{version = Version,
                          path = Path,
                          mounted = true}};
        {Status, Output} ->
            {error, {path_v1, Status, Output}}
    end;
new_state_init(2 = Version, false, Path) ->
    case shell("mount -t cgroup2 none \"~s\"", [Path]) of
        {0, _} ->
            {ok, #cgroups{version = Version,
                          path = Path,
                          mounted = true}};
        {Status, Output} ->
            {error, {path_v2, Status, Output}}
    end.

new_paths(PathV1, PathV2, undefined) ->
    {ok, {false, PathV1}, {false, PathV2}};
new_paths(PathV1, PathV2, PathMounts) ->
    case shell("cat \"~s\"", [PathMounts]) of
        {0, Mounts} ->
            MountsStr = erlang:binary_to_list(erlang:iolist_to_binary(Mounts)),
            MountsL = string:tokens(MountsStr, "\n"),
            {MountsPathV1,
             MountsPathV2} = new_mounts(MountsL),
            ResultV1 = if
                MountsPathV1 =:= undefined ->
                    {false, PathV1};
                is_list(MountsPathV1) ->
                    {true, MountsPathV1}
            end,
            ResultV2 = if
                MountsPathV2 =:= undefined ->
                    {false, PathV2};
                is_list(MountsPathV2) ->
                    {true, MountsPathV2}
            end,
            {ok, ResultV1, ResultV2};
        {Status, Output} ->
            {error, {path_mounts, Status, Output}}
    end.

new_mounts(MountsL) ->
    new_mounts(MountsL, undefined, undefined).

new_mounts([], PathV1, PathV2) ->
    {PathV1, PathV2};
new_mounts([Mount | MountsL], PathV1, PathV2) ->
    case string:tokens(Mount, " ") of
        [_, NewPathV1, "cgroup" | _] ->
            new_mounts(MountsL, NewPathV1 ++ "/", PathV2);
        [_, NewPathV2, "cgroup2" | _] ->
            new_mounts(MountsL, PathV1, NewPathV2 ++ "/");
        _ ->
            new_mounts(MountsL, PathV1, PathV2)
    end.

subsystem([_ | _] = SubsystemParameter) ->
    {Subsystem, _} = lists:splitwith(fun(C) -> C /= $. end, SubsystemParameter),
    Subsystem.

update_parameters(CGroupParameters, 1, CGroupPathFull) ->
    update_parameters(CGroupParameters, CGroupPathFull);
update_parameters(CGroupParameters, 2, CGroupPathFull) ->
    Controllers = lists:usort([subsystem(SubsystemParameter)
                               || {SubsystemParameter, _} <- CGroupParameters]),
    ControlAdded = ["+" ++ Controller || Controller <- Controllers],
    ControlRemoved = ["-" ++ Controller || Controller <- Controllers],
    CGroupSubPathFull = filename:dirname(CGroupPathFull) ++ "/",
    case shell("echo \"~s\" > \"~s/cgroup.subtree_control\"",
               [string:join(ControlAdded, " "),
                CGroupSubPathFull]) of
        {0, _} ->
            Result = update_parameters(CGroupParameters, CGroupPathFull),
            case shell("echo \"~s\" > \"~s/cgroup.subtree_control\"",
                       [string:join(ControlRemoved, " "),
                        CGroupSubPathFull]) of
                {0, _} ->
                    Result;
                {Status, Output} ->
                    {error, {subtree_control, Status, Output}}
            end;
        {Status, Output} ->
            {error, {subtree_control, Status, Output}}
    end.

update_parameters([], _) ->
    ok;
update_parameters([{SubsystemParameter, Value} | CGroupParameters],
                  CGroupPathFull) ->
    case shell("echo \"~s\" > \"~s/~s\"",
               [Value, CGroupPathFull, SubsystemParameter]) of
        {0, _} ->
            update_parameters(CGroupParameters, CGroupPathFull);
        {Status, Output} ->
            {error, {subsystem_parameter, Status, Output}}
    end.

update_pids(OSPids, _, CGroupPathFull) ->
    update_pids(OSPids, CGroupPathFull).

update_pids([], _) ->
    ok;
update_pids([OSPid | OSPids], CGroupPathFull) ->
    case shell("echo \"~w\" > \"~s/cgroup.procs\"",
               [OSPid, CGroupPathFull]) of
        {0, _} ->
            update_pids(OSPids, CGroupPathFull);
        {Status, Output} ->
            {error, {procs, Status, Output}}
    end.

cgroup_path_valid([_ | _] = CGroupPath) ->
    ($/ /= hd(CGroupPath)) andalso
    ($/ /= hd(lists:reverse(CGroupPath))).

option(Key, Options) ->
    case lists:keytake(Key, 1, Options) of
        false ->
            {ok, Value} = application:get_env(?APPLICATION, Key),
            {Value, Options};
        {value, {Key, Value}, NewOptions} ->
            {Value, NewOptions}
    end.

shell_output(Shell, Output) ->
    receive
        {Shell, {data, Data}} ->
            shell_output(Shell, [Data | Output]);
        {Shell, {exit_status, Status}} ->
            {Status, lists:reverse(Output)}
    end.

