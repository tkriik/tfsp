%%% tfsp supervisor, starts multiple sync transports over a single
%%% file system context.
-module(tfsp_sup).
-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-include("fs.hrl").
-include("transport.hrl").


%%% Constants

-define(SUP_NAME, ?MODULE).

%% TODO: pass these from elsewhere
-define(SCAN_INTERVAL, 60000).
-define(SCAN_IGNORE_RES, []).

-define(CONNECT_TIMEOUT, 60000).
-define(SHUTDOWN_TIMEOUT, 5000).


%%% API

start_link(FsCtx, SyncTransports) ->
    supervisor:start_link(?MODULE, [FsCtx, SyncTransports]).


%%% supervisor callbacks

init([#fs_ctx{ ev_mgr_ref = EvMgrRef, root = Root} = FsCtx, SyncTransports]) ->
    tfsp_event:notify_tfsp_sup_up(EvMgrRef, Root),
    SupFlags = #{ strategy  => one_for_one,
                  intensity => 1,
                  period    => 5 },
    % TODO: separate scanner supervisor
    ScannerSpec = #{ id         => make_ref(),
                     start      => {tfsp_scanner, start_link, [FsCtx,
                                                               ?SCAN_INTERVAL,
                                                               ?SCAN_IGNORE_RES]},
                     restart    => permanent,
                     shutdown   => brutal_kill,
                     type       => worker },
    TransportSpecs = lists:map(fun(Transport) ->
                                       transport_to_child_spec(FsCtx, Transport)
                               end, SyncTransports),
    ChildSpecs = lists:append([ScannerSpec], TransportSpecs),
    {ok, {SupFlags, ChildSpecs}}.


%%% Utilities

transport_to_child_spec(FsCtx, #ssh_connect_transport{ host    = Host,
                                                       port    = Port,
                                                       options = Options }) ->
    #{ id       => make_ref(),
       start    => {tfsp_client, start_link_ssh, [FsCtx,
                                                  Host,
                                                  Port,
                                                  ?CONNECT_TIMEOUT,
                                                  Options]},
       restart  => permanent,
       shutdown => 5000,
       type     => worker };
%% TODO: just use SSH options instead of hard-coded system and user dir
transport_to_child_spec(FsCtx, #ssh_serve_transport{ port       = Port,
                                                     system_dir = SystemDir,
                                                     user_dir   = UserDir }) ->
    #{ id       => make_ref(),
       start    => {tfsp_server, start_link_ssh, [FsCtx, Port, SystemDir, UserDir]},
       restart  => permanent,
       shutdown => 5000,
       type     => worker }.
