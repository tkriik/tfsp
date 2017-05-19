%%% tfsp SSH server module, handles connection data buffering
%%% and communicating incoming and outgoing data to/from other
%%% handler processes.
-module(tfsp_ssh_server).
-behaviour(ssh_daemon_channel).

-export([start_daemon/5,
         stop_daemon/1]).

-export([init/1,
         handle_ssh_msg/2,
         handle_msg/2,
         terminate/2]).

-include("conn.hrl").
-include("fs.hrl").


%% Specs

-spec start_daemon(fs_ent_tab(),
                   fs_path(),
                   non_neg_integer(),
                   fs_path(),
                   fs_path()) -> {ok, ssh:ssh_daemon_ref()} | {error, atom()}.
-spec stop_daemon(ssh:ssh_daemon_ref()) -> ok.


%% API

start_daemon(Table, Root, Port, SystemDir, UserDir) ->
    SubsystemSpec = {"tfsp_ssh_server", {tfsp_ssh_server, [Table, Root]}},
    SshOpts = [{subsystems, [SubsystemSpec]},
               {ssh_cli, no_cli},
               {system_dir, SystemDir},
               {user_dir, UserDir},
               {auth_methods, "publickey"}],
    ssh:daemon(Port, SshOpts).

stop_daemon(DaemonRef) ->
    ssh:stop_daemon(DaemonRef).


%% SSH daemon channel allbacks

init([_Table, _Root]) ->
    St = #conn_st{ buffer = <<>> },
    {ok, St}.

handle_msg(_Msg, St) ->
    {ok, St}.

% Got data from channel, append to channel state buffer.
handle_ssh_msg({ssh_cm, _ConnRef, {data, _ChanId, 0, Data}},
               #conn_st { buffer = Buffer } = St) ->
    _Buffer = <<Buffer/binary, Data/binary>>,
    _St = St#conn_st{ buffer = _Buffer },
    {ok, _St};
%% Got EOF from channel, flush channel state buffer.
handle_ssh_msg({ssh_cm, _ConnRef, {eof, _ChanId}}, St) ->
    _St = St#conn_st{ buffer = <<>> },
    {ok, _St};
handle_ssh_msg(_Msg, St) ->
    {ok, St}.

terminate(_Reason, _St) ->
    ok.
