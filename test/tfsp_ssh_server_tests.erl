%%% Tests for the SSH server module interface.

-module(tfsp_ssh_server_tests).

-include_lib("eunit/include/eunit.hrl").


%% Constants

-define(PORT, 1234).
-define(SYSTEM_DIR, "test/data/ssh_server/etc/ssh").


%% Main tests

module_test_() ->
    {"Tests for the tfsp SSH server module interface",
     [{setup, "server setup and teardown", fun app_setup/0, fun test_start_stop/0}]
    }.


%% Test definitions

test_start_stop() ->
    Result = tfsp_ssh_server:start(?PORT, ?SYSTEM_DIR),
    ?assertMatch({ok, _}, Result),
    {ok, DaemonRef} = Result,
    ?assertEqual(ok, tfsp_ssh_server:stop(DaemonRef)).


%% Fixtures

app_setup() ->
    {ok, _} = application:ensure_all_started(ssh).

setup() ->
    app_setup(),
    {ok, DaemonRef} = tfsp_ssh_server:start(?PORT, ?SYSTEM_DIR),
    DaemonRef.

cleanup(DaemonRef) ->
    ok = tfsp_ssh_server:stop(DaemonRef).
