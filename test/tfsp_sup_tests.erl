%%% tfsp supervisor tests, mainly implemented with event sequence verification.
-module(tfsp_sup_tests).

-include_lib("eunit/include/eunit.hrl").

-include("fs.hrl").
-include("ssh_defs.hrl").
-include("transport.hrl").


%%% Specs

-record(fixture_st, { client_sup_ref    :: pid(),
                      client_fs_ctx     :: fs_ctx(),
                      server_sup_ref    :: pid(),
                      server_fs_ctx     :: fs_ctx() }).


%%% Main tests

sync_test_() ->
    {"tfsp supervisor module interface",
     {foreach, fun setup/0, fun cleanup/1,
      [fun test_ssh_sync/1]}
    }.


%%% Test definitions

test_ssh_sync(#fixture_st{ client_fs_ctx = #fs_ctx{ ev_mgr_ref = ClientEvMgrRef },
                           server_fs_ctx = #fs_ctx{ ev_mgr_ref = ServerEvMgrRef } }) ->
    ServerSpecs = [{tfsp_sup_up, path:normalize_root(?SSH_SERVER_ROOT)},
                   ssh_server_chan_up
                  ],
    ClientSpecs = [{tfsp_sup_up, path:normalize_root(?SSH_CLIENT_ROOT)},
                   ssh_client_chan_up
                  ],
    timer:sleep(100), % wait for events to arrive
    {"tests client-server synchronization over SSH",
     [?_assertEqual(ok, event_queue:verify_strict(ClientEvMgrRef, ClientSpecs)),
      ?_assertEqual(ok, event_queue:verify_strict(ServerEvMgrRef, ServerSpecs))]}.


%%% Fixtures

setup() ->
    {ok, _} = application:ensure_all_started(ssh),
    ServerFsCtx = mk_fs_ctx(?SSH_SERVER_ROOT),
    {ok, ServerSupRef} = tfsp_sup:start_link(ServerFsCtx, [ssh_serve_transport()]),
    % TODO: make possible to launch client even with server down
    ClientFsCtx = mk_fs_ctx(?SSH_CLIENT_ROOT),
    {ok, ClientSupRef} = tfsp_sup:start_link(ClientFsCtx, [ssh_connect_transport()]),
    #fixture_st{ client_sup_ref = ClientSupRef,
                 client_fs_ctx  = ClientFsCtx,
                 server_sup_ref = ServerSupRef,
                 server_fs_ctx  = ServerFsCtx }.

cleanup(#fixture_st{ client_sup_ref = ClientSupRef,
                     client_fs_ctx  = ClientFsCtx,
                     server_sup_ref = ServerSupRef,
                     server_fs_ctx  = ServerFsCtx }) ->
    true = unlink(ClientSupRef),
    true = unlink(ServerSupRef),
    true = exit(ClientSupRef, shutdown),
    true = exit(ServerSupRef, shutdown),
    ok = del_fs_ctx(ClientFsCtx),
    ok = del_fs_ctx(ServerFsCtx),
    timer:sleep(5000). 


%%% Utilities

mk_fs_ctx(Root) ->
    {ok, EvMgrRef} = event_queue:start_link(),
    _Root = path:normalize_root(Root),
    EntTab = fs_ent_tab:create(),
    #fs_ctx{ ev_mgr_ref = EvMgrRef,
             root       = _Root,
             ent_tab    = EntTab }.

del_fs_ctx(#fs_ctx{ ev_mgr_ref  = EvMgrRef,
                    ent_tab     = EntTab }) ->
    ok = event_queue:stop(EvMgrRef),
    ok = fs_ent_tab:delete(EntTab).

%%% Test transports

ssh_connect_transport() ->
    #ssh_connect_transport{ host    = ?SSH_HOST,
                            port    = ?SSH_PORT,
                            options = ?SSH_CLIENT_OPTS }.

ssh_serve_transport() ->
    #ssh_serve_transport{ port          = ?SSH_PORT,
                          system_dir    = ?SSH_SERVER_SYSTEM_DIR,
                          user_dir      = ?SSH_SERVER_USER_DIR }.
