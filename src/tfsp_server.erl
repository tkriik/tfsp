-module(tfsp_server).
-behaviour(gen_server).

-export([start_link_ssh/4,
         stop/1]).

-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-include("fs.hrl").


%%% Specs

-record(ssh_init_args, { fs_ctx     :: fs_ctx(),
                         port       :: non_neg_integer(),
                         system_dir :: fs_path(),
                         user_dir   :: fs_path() }).

-type ssh_init_args() :: #ssh_init_args{}.
-type init_args() :: ssh_init_args().

-record(ssh_server_st, { daemon_ref :: ssh:ssh_daemon_ref() }).

-type ssh_server_st() :: #ssh_server_st{}.
-type server_st() :: ssh_server_st().

-spec start_link_ssh(fs_ctx(),
                     non_neg_integer(),
                     string(),
                     string()) -> {ok, pid()} | {error, term()}.
-spec stop(pid()) -> ok.

-spec init(init_args()) -> {ok, server_st()} | {stop, term()}.


%%% API

start_link_ssh(FsCtx, Port, SystemDir, UserDir) ->
    SshInitArgs = #ssh_init_args{ fs_ctx        = FsCtx,
                                  port          = Port,
                                  system_dir    = SystemDir,
                                  user_dir      = UserDir },
    gen_server:start_link(?MODULE, SshInitArgs, []).

stop(ServerRef) ->
    gen_server:stop(ServerRef).


%%% SSH-specific gen_server callbacks

init(#ssh_init_args{ fs_ctx     = FsCtx,
                     port       = Port,
                     system_dir = SystemDir,
                     user_dir   = UserDir }) ->
    case tfsp_ssh_server:start_daemon(FsCtx, Port, SystemDir, UserDir) of
        {ok, DaemonRef} ->
            process_flag(trap_exit, true),
            SshServerSt = #ssh_server_st{ daemon_ref = DaemonRef },
            {ok, SshServerSt};
        {error, Reason} ->
            {stop, Reason}
    end.

terminate(_Reason, #ssh_server_st{ daemon_ref = DaemonRef }) ->
    ok = ssh:stop_daemon(DaemonRef);
terminate(_Reason, _St) ->
    ok.

handle_call(_Request, _From, #ssh_server_st{} = St) ->
    {noreply, St}.

handle_cast(_Request, #ssh_server_st{} = St) ->
    {noreply, St}.

handle_info(_Info, #ssh_server_st{} = St) ->
    {noreply, St}.

code_change(_OldVsn, #ssh_server_st{} = St, _Extra) ->
    {ok, St}.
