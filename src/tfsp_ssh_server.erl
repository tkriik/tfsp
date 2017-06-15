%%% tfsp SSH server module, handles connection data buffering
%%% and communicating incoming and outgoing data to/from other
%%% handler processes.
-module(tfsp_ssh_server).
-behaviour(ssh_daemon_channel).

-export([start_daemon/4,
         stop_daemon/1,
         subsystem_name/0]).

-export([init/1,
         handle_ssh_msg/2,
         handle_msg/2,
         terminate/2]).

-include("conn.hrl").
-include("fs.hrl").


%%% Specs

-spec start_daemon(fs_ctx(),
                   non_neg_integer(),
                   fs_path(),
                   fs_path()) -> {ok, ssh:ssh_daemon_ref()} | {error, atom()}.
-spec stop_daemon(ssh:ssh_daemon_ref()) -> ok.


%%% API

start_daemon(FsCtx, Port, SystemDir, UserDir) ->
    SubsystemSpec = {subsystem_name(), {?MODULE, [FsCtx]}},
    SshOpts = [{subsystems, [SubsystemSpec]},
               {ssh_cli, no_cli},
               {system_dir, SystemDir},
               {user_dir, UserDir},
               {auth_methods, "publickey"}],
    ssh:daemon(Port, SshOpts).

stop_daemon(DaemonRef) ->
    ssh:stop_daemon(DaemonRef).

subsystem_name() ->
    ?MODULE_STRING.


%%% SSH daemon channel callbacks

init([FsCtx]) ->
    St = #conn_st{ fs_ctx = FsCtx,
                   buffer = <<>> },
    {ok, St}.

handle_msg({ssh_channel_up, _ChanId, _ConnRef},
           #conn_st{ fs_ctx = #fs_ctx{ ev_mgr_ref = EvMgrRef } } = St) ->
    tfsp_event:notify_ssh_server_chan_up(EvMgrRef, undefined),
    {ok, St};
handle_msg(_Msg, St) ->
    {ok, St}.

%% Got data from channel, append to channel state buffer.
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
