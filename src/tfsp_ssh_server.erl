%%% tfsp SSH server module, handles buffering, serialization/deserialization
%%% and communicating incoming and outgoing data to/from other handler processes.
-module(tfsp_ssh_server).
-behaviour(ssh_daemon_channel).

-export([start_daemon/5,
         stop_daemon/1]).

-export([init/1,
         handle_ssh_msg/2,
         handle_msg/2,
         terminate/2]).


%% Records

-record(ssh_server_st, { ftab   :: fs_ent_tab:handle(),
                         root   :: file:path(),
                         buffer :: binary() }).


%% Specs

-type ssh_server_st() :: #ssh_server_st{}.

-spec start_daemon(fs_ent_tab:handle(),
                   file:path(),
                   non_neg_integer(),
                   file:path(),
                   file:path()) -> {ok, ssh:ssh_daemon_ref()} | {error, atom()}.
-spec stop_daemon(ssh:ssh_daemon_ref()) -> ok.

-spec init(term()) -> {ok, ssh_server_st()}.
-spec handle_ssh_msg(ssh_connection:event(),
                     ssh_server_st()) -> {ok, ssh_server_st()}.
-spec handle_msg(term(), ssh_server_st()) -> {ok, ssh_server_st()}.
-spec terminate(term(), ssh_server_st()) -> ok.


%% API

start_daemon(Table, Root, Port, SystemDir, UserDir) ->
    % TODO: log
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

init([Table, Root]) ->
    % TODO: log
    St = #ssh_server_st{ ftab = Table,
                         root = Root,
                         buffer = <<>> },
    {ok, St}.
handle_msg({ssh_channel_up, _ChanId, _ConnRef}, St) ->
    % TODO: log
    {ok, St};
handle_msg(_Msg, St) ->
    % TODO: log
    {ok, St}.

handle_ssh_msg({data, _ChanId, 0, Data},
               #ssh_server_st{ buffer = Buffer } = St) ->
    _Buffer = <<Buffer/binary, Data/binary>>,
    _St = St#ssh_server_st{ buffer = _Buffer },
    {ok, _St};
handle_ssh_msg({eof, _ChanId}, St) ->
    _St = St#ssh_server_st{ buffer = <<>> },
    {ok, _St};
handle_ssh_msg(_Msg, St) ->
    {ok, St}.

terminate(_Reason, _St) ->
    % TODO: log
    ok.
