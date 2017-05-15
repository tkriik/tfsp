%%% tfsp SSH server module, handles buffering, serialization/deserialization
%%% and communicating incoming and outgoing data to/from other handler processes.
-module(tfsp_ssh_server).
-behaviour(ssh_daemon_channel).

-export([start_daemon/3,
         stop_daemon/1]).

-export([init/1,
         handle_ssh_msg/2,
         handle_msg/2,
         terminate/2]).


%% Records

-record(ssh_server_st, { buffer }).


%% Specs

-type ssh_server_st() :: #ssh_server_st{}.

-spec start_daemon(non_neg_integer(), file:path(), file:path()) -> {ok, ssh:ssh_daemon_ref()} | {error, atom()}.
-spec stop_daemon(ssh:ssh_daemon_ref()) -> ok.

-spec init(term()) -> {ok, ssh_server_st()}.
-spec handle_ssh_msg(ssh_connection:event(), ssh_server_st()) -> {ok, ssh_server_st()}.
-spec handle_msg(term(), ssh_server_st()) -> {ok, ssh_server_st()}.
-spec terminate(term(), ssh_server_st()) -> ok.


%% API

start_daemon(Port, SystemDir, UserDir) ->
    % TODO: log
    SubsystemSpec = {erlang:atom_to_list(?MODULE), {?MODULE, []}},
    SshOpts = [{subsystems, [SubsystemSpec]},
               {ssh_cli, no_cli},
               {system_dir, SystemDir},
               {user_dir, UserDir},
               {auth_methods, "publickey"}],
    ssh:daemon(Port, SshOpts).

stop_daemon(DaemonRef) ->
    ssh:stop_daemon(DaemonRef).


%% SSH daemon channel allbacks

init(_Args) ->
    % TODO: log
    St = #ssh_server_st{ buffer = <<>> },
    {ok, St}.

handle_msg({ssh_channel_up, _ChanId, _ConnRef}, St) ->
    % TODO: log
    {ok, St};
handle_msg(_Msg, St) ->
    % TODO: log
    {ok, St}.

handle_ssh_msg(_Msg, St) ->
    {ok, St}.

terminate(_Reason, _St) ->
    % TODO: log
    ok.
