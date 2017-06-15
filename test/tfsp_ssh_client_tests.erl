%%% Tests for the SSH client module interface, via tfsp_client.

-module(tfsp_ssh_client_tests).

-include_lib("eunit/include/eunit.hrl").

-include("fs.hrl").
-include("ssh_defs.hrl").


%% Specs

-record(fixture_st, { ev_mgr_ref    :: pid(),
                      server    :: pid(),
                      client    :: pid() }).


%% Main tests

client_test_() ->
    {"Tests for the tfsp SSH client module interface",
     {foreach, fun setup/0, fun cleanup/1,
      [fun client_event_test/1]}
    }.


%% Test definitions

client_event_test(#fixture_st { ev_mgr_ref = EvMgrRef }) ->
    Specs = [ssh_client_chan_up],
    {"client event sequence",
     ?_assertEqual(ok, event_queue:verify_strict(EvMgrRef, Specs))}.


%% Fixtures

setup() ->
    {ok, _} = application:ensure_all_started(ssh),
    {ok, EvMgrRef} = event_queue:start_link(),

    ServerFsCtx = #fs_ctx{ root = path:normalize_root(?SSH_SERVER_ROOT) },
    {ok, ServerRef} = tfsp_server:start_link_ssh(ServerFsCtx,
                                                 ?SSH_PORT,
                                                 ?SSH_SERVER_SYSTEM_DIR,
                                                 ?SSH_SERVER_USER_DIR),
    ClientFsCtx = #fs_ctx{ ev_mgr_ref = EvMgrRef,
                           root   = path:normalize_root(?SSH_CLIENT_ROOT) },
    {ok, ClientRef} = tfsp_client:start_link_ssh(ClientFsCtx,
                                                 ?SSH_HOST,
                                                 ?SSH_PORT,
                                                 ?SSH_TIMEOUT,
                                                 ?SSH_CLIENT_OPTS),
    #fixture_st{ ev_mgr_ref = EvMgrRef,
                 server = ServerRef,
                 client = ClientRef }.

cleanup(#fixture_st{ ev_mgr_ref = EvMgrRef,
                     server = ServerRef,
                     client = ClientRef }) ->
    ok = event_queue:stop(EvMgrRef),
    ok = tfsp_server:stop(ServerRef),
    ok = tfsp_client:stop(ClientRef),
    ok = file:delete(?SSH_CLIENT_KNOWN_HOSTS).
