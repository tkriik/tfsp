%%% Tests for the SSH server module interface.

-module(tfsp_ssh_server_tests).

-include_lib("eunit/include/eunit.hrl").


%% Constants

-define(HOST, "localhost").
-define(PORT, 1234).
-define(SERVER_SYSTEM_DIR, "test/data/ssh_server/etc/ssh/").
-define(SERVER_USER_DIR, "test/data/ssh_server/home/ssh_server/ssh").
-define(CLIENT_USER_DIR, "test/data/ssh_client/home/ssh_client/ssh/").
-define(CLIENT_KNOWN_HOSTS, "test/data/ssh_client/home/ssh_client/ssh/known_hosts").


%% Main tests

module_test_() ->
    {"Tests for the tfsp SSH server module interface",
     [daemon_tests(),
      conn_tests()]}.

daemon_tests() ->
    {setup, fun app_setup/0,
     {"daemon setup and teardown", fun test_start_stop/0}}.

conn_tests() ->
    {foreach, fun setup/0, fun cleanup/1,
     [{"connection setup and teardown", fun test_connection/0},
      {"channel setup and teardown", fun test_channel/0},
      {"subsystem execution request", fun test_subsystem/0}]
    }.


%% Test definitions

test_start_stop() ->
    Res = tfsp_ssh_server:start(?PORT, ?SERVER_SYSTEM_DIR, ?SERVER_USER_DIR),
    ?assertMatch({ok, _}, Res),
    {ok, DaemonRef} = Res,
    ?assertEqual(ok, tfsp_ssh_server:stop(DaemonRef)).

test_connection() ->
    Res = ssh:connect(?HOST, ?PORT, client_ssh_opts(), 1000),
    ?assertMatch({ok, _}, Res),
    {ok, ConnRef} = Res,
    ?assertEqual(ok, ssh:close(ConnRef)).

test_channel() ->
    {ok, ConnRef} = ssh:connect(?HOST, ?PORT, client_ssh_opts(), 1000),
    Res = ssh_connection:session_channel(ConnRef, 32768, 65536, 1000),
    ?assertMatch({ok, _}, Res),
    {ok, ChanId} = Res,
    ?assertEqual(ok, ssh_connection:close(ConnRef, ChanId)),
    ?assertEqual(ok, ssh:close(ConnRef)).

test_subsystem() ->
    {ok, ConnRef} = ssh:connect(?HOST, ?PORT, client_ssh_opts(), 1000),
    {ok, ChanId} = ssh_connection:session_channel(ConnRef, 32768, 65536, 1000),
    Res = ssh_connection:subsystem(ConnRef, ChanId, "tfsp_ssh_server", 1000),
    ?assertEqual(success, Res),
    ?assertEqual(ok, ssh_connection:close(ConnRef, ChanId)),
    ?assertEqual(ok, ssh:close(ConnRef)).


%% Fixtures

app_setup() ->
    {ok, _} = application:ensure_all_started(ssh).

setup() ->
    {ok, DaemonRef} = tfsp_ssh_server:start(?PORT, ?SERVER_SYSTEM_DIR, ?SERVER_USER_DIR),
    DaemonRef.

cleanup(DaemonRef) ->
    _ = file:delete(?CLIENT_KNOWN_HOSTS),
    ok = tfsp_ssh_server:stop(DaemonRef).

%% Utilities

client_ssh_opts() ->
    [{user_dir, ?CLIENT_USER_DIR},
     {user_interaction, false},
     {silently_accept_hosts, true},
     {user, "ssh_client"}].
