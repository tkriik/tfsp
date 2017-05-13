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
     [{"server setup and teardown",
       {setup, fun app_setup/0, fun test_start_stop/0}
      },
      {"SSH client connection setup and teardown",
       {setup, fun setup/0, fun cleanup/1, fun test_connection/0}
      }
     ]
    }.


%% Test definitions

test_start_stop() ->
    Res = tfsp_ssh_server:start(?PORT, ?SERVER_SYSTEM_DIR, ?SERVER_USER_DIR),
    ?assertMatch({ok, _}, Res),
    {ok, DaemonRef} = Res,
    ?assertEqual(ok, tfsp_ssh_server:stop(DaemonRef)).

test_connection() ->
    Opts = [{user_dir, ?CLIENT_USER_DIR},
            {user_interaction, false},
            {silently_accept_hosts, true},
            {user, "ssh_client"}],
    Res = ssh:connect(?HOST, ?PORT, Opts, 1000),
    ?assertMatch({ok, _}, Res),
    {ok, ConnRef} = Res,
    ?assertEqual(ok, ssh:close(ConnRef)).


%% Fixtures

app_setup() ->
    {ok, _} = application:ensure_all_started(ssh).

setup() ->
    app_setup(),
    {ok, DaemonRef} = tfsp_ssh_server:start(?PORT, ?SERVER_SYSTEM_DIR, ?SERVER_USER_DIR),
    DaemonRef.

cleanup(DaemonRef) ->
    _ = file:delete(?CLIENT_KNOWN_HOSTS),
    ok = tfsp_ssh_server:stop(DaemonRef).
