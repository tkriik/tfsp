%%% Tests for the SSH server module interface.

-module(tfsp_ssh_server_tests).

-include_lib("eunit/include/eunit.hrl").


%% Constants

-define(ROOT, <<"test/data/ssh_server">>).
-define(HOST, "localhost").
-define(PORT, 1234).
-define(SERVER_SYSTEM_DIR, "test/data/ssh_server/etc/ssh/").
-define(SERVER_USER_DIR, "test/data/ssh_server/home/ssh_server/ssh").
-define(CLIENT_USER_DIR, "test/data/ssh_client/home/ssh_client/ssh/").
-define(CLIENT_KNOWN_HOSTS, "test/data/ssh_client/home/ssh_client/ssh/known_hosts").
-define(TIMEOUT, 5000).


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
    FsTab = fs_ent_tab:create(),
    Root = path:normalize_root(?ROOT),
    Res = tfsp_server:start_link_ssh(FsTab, Root, ?PORT, ?SERVER_SYSTEM_DIR, ?SERVER_USER_DIR),
    ?assertMatch({ok, _}, Res),
    {ok, ServerRef} = Res,
    ?assertEqual(ok, tfsp_server:stop(ServerRef)).

test_connection() ->
    Res = ssh:connect(?HOST, ?PORT, client_ssh_opts(), ?TIMEOUT),
    ?assertMatch({ok, _}, Res),
    {ok, ConnRef} = Res,
    ?assertEqual(ok, ssh:close(ConnRef)).

test_channel() ->
    {ok, ConnRef} = ssh:connect(?HOST, ?PORT, client_ssh_opts(), ?TIMEOUT),
    Res = ssh_connection:session_channel(ConnRef, 32768, 65536, ?TIMEOUT),
    ?assertMatch({ok, _}, Res),
    {ok, ChanId} = Res,
    ?assertEqual(ok, ssh_connection:close(ConnRef, ChanId)),
    ?assertEqual(ok, ssh:close(ConnRef)).

test_subsystem() ->
    {ok, ConnRef} = ssh:connect(?HOST, ?PORT, client_ssh_opts(), ?TIMEOUT),
    {ok, ChanId} = ssh_connection:session_channel(ConnRef, 32768, 65536, ?TIMEOUT),
    Res = ssh_connection:subsystem(ConnRef, ChanId, "tfsp_ssh_server", ?TIMEOUT),
    ?assertEqual(success, Res),
    ?assertEqual(ok, ssh_connection:close(ConnRef, ChanId)),
    ?assertEqual(ok, ssh:close(ConnRef)).

test_client_send() ->
    {ok, ConnRef} = ssh:connect(?HOST, ?PORT, client_ssh_opts(), ?TIMEOUT),
    {ok, ChanId} = ssh_connection:session_channel(ConnRef, 32768, 65536, ?TIMEOUT),
    success = ssh_connection:subsystem(ConnRef, ChanId, "tfsp_ssh_server", ?TIMEOUT),
    ?assertEqual(ok, ssh_connection:send(ConnRef, ChanId, <<"FOO">>, ?TIMEOUT)),
    ?assertEqual(ok, ssh_connection:send(ConnRef, ChanId, <<"BAR">>, ?TIMEOUT)),
    ?assertEqual(ok, ssh_connection:send_eof(ConnRef, ChanId)),
    timer:sleep(100), % wait for EOF to arrive
    ?assertEqual(ok, ssh:close(ConnRef)).


%% Fixtures

app_setup() ->
    {ok, _} = application:ensure_all_started(ssh).

setup() ->
    FsTab = fs_ent_tab:create(),
    Root = path:normalize_root(?ROOT),
    {ok, ServerRef} = tfsp_server:start_link_ssh(FsTab, Root, ?PORT,
                                                 ?SERVER_SYSTEM_DIR,
                                                 ?SERVER_USER_DIR),
    ServerRef.

cleanup(ServerRef) ->
    _ = file:delete(?CLIENT_KNOWN_HOSTS),
    ok = tfsp_server:stop(ServerRef).

%% Utilities

client_ssh_opts() ->
    [{user_dir, ?CLIENT_USER_DIR},
     {user_interaction, false},
     {silently_accept_hosts, true},
     {user, "ssh_client"}].
