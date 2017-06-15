%%% Tests for the SSH server module interface.

-module(tfsp_ssh_server_tests).

-include_lib("eunit/include/eunit.hrl").

-include("fs.hrl").
-include("ssh_defs.hrl").


%% Main tests


module_test_() ->
    {"Tests for the tfsp SSH server module interface",
     [server_tests(),
      conn_tests()]}.

server_tests() ->
    {setup, fun app_setup/0,
     {"server setup and teardown", fun test_start_stop/0}}.

conn_tests() ->
    {foreach, fun setup/0, fun cleanup/1,
     [{"connection setup and teardown", fun test_connection/0},
      {"channel setup and teardown", fun test_channel/0},
      {"subsystem execution request", fun test_subsystem/0},
      {"client connection data send", fun test_client_send/0}]
    }.


%% Test definitions

test_start_stop() ->
    EvMgrRef = event_queue:start_link(),
    Root = path:normalize_root(?SSH_SERVER_ROOT),
    EntTab = fs_ent_tab:create(),
    FsCtx = #fs_ctx{ ev_mgr_ref = EvMgrRef,
                     ent_tab    = EntTab,
                     root       = Root },
    Res = tfsp_server:start_link_ssh(FsCtx,
                                     ?SSH_PORT,
                                     ?SSH_SERVER_SYSTEM_DIR,
                                     ?SSH_SERVER_USER_DIR),
    ?assertMatch({ok, _}, Res),
    {ok, ServerRef} = Res,
    ?assertEqual(ok, tfsp_server:stop(ServerRef)).

test_connection() ->
    Res = ssh:connect(?SSH_HOST, ?SSH_PORT, ?SSH_CLIENT_OPTS, ?SSH_TIMEOUT),
    ?assertMatch({ok, _}, Res),
    {ok, ConnRef} = Res,
    ?assertEqual(ok, ssh:close(ConnRef)).

test_channel() ->
    {ok, ConnRef} = ssh:connect(?SSH_HOST, ?SSH_PORT, ?SSH_CLIENT_OPTS, ?SSH_TIMEOUT),
    Res = ssh_connection:session_channel(ConnRef, 32768, 65536, ?SSH_TIMEOUT),
    ?assertMatch({ok, _}, Res),
    {ok, ChanId} = Res,
    ?assertEqual(ok, ssh_connection:close(ConnRef, ChanId)),
    ?assertEqual(ok, ssh:close(ConnRef)).

test_subsystem() ->
    {ok, ConnRef} = ssh:connect(?SSH_HOST, ?SSH_PORT, ?SSH_CLIENT_OPTS, ?SSH_TIMEOUT),
    {ok, ChanId} = ssh_connection:session_channel(ConnRef, 32768, 65536, ?SSH_TIMEOUT),
    Res = ssh_connection:subsystem(ConnRef, ChanId, tfsp_ssh_server:subsystem_name(), ?SSH_TIMEOUT),
    ?assertEqual(success, Res),
    ?assertEqual(ok, ssh_connection:close(ConnRef, ChanId)),
    ?assertEqual(ok, ssh:close(ConnRef)).

test_client_send() ->
    {ok, ConnRef} = ssh:connect(?SSH_HOST, ?SSH_PORT, ?SSH_CLIENT_OPTS, ?SSH_TIMEOUT),
    {ok, ChanId} = ssh_connection:session_channel(ConnRef, 32768, 65536, ?SSH_TIMEOUT),
    success = ssh_connection:subsystem(ConnRef, ChanId, tfsp_ssh_server:subsystem_name(), ?SSH_TIMEOUT),
    ?assertEqual(ok, ssh_connection:send(ConnRef, ChanId, <<"FOO">>, ?SSH_TIMEOUT)),
    ?assertEqual(ok, ssh_connection:send(ConnRef, ChanId, <<"BAR">>, ?SSH_TIMEOUT)),
    ?assertEqual(ok, ssh_connection:send_eof(ConnRef, ChanId)),
    timer:sleep(100), % wait for EOF to arrive
    ?assertEqual(ok, ssh:close(ConnRef)).


%% Fixtures

app_setup() ->
    {ok, _} = application:ensure_all_started(ssh).

setup() ->
    EvMgrRef = event_queue:start_link(),
    EntTab = fs_ent_tab:create(),
    Root = path:normalize_root(?SSH_SERVER_ROOT),
    FsCtx = #fs_ctx{ ev_mgr_ref = EvMgrRef,
                     ent_tab    = EntTab,
                     root       = Root },
    {ok, ServerRef} = tfsp_server:start_link_ssh(FsCtx,
                                                 ?SSH_PORT,
                                                 ?SSH_SERVER_SYSTEM_DIR,
                                                 ?SSH_SERVER_USER_DIR),
    ServerRef.

cleanup(ServerRef) ->
    _ = file:delete(?SSH_CLIENT_KNOWN_HOSTS),
    ok = tfsp_server:stop(ServerRef).
