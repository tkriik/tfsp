%%% Tests for the tfsp protocol handler module interface.
-module(tfsp_proto_tests).

-include_lib("eunit/include/eunit.hrl").

-include("fs.hrl").


%% Main tests

module_test_() ->
    {"tfsp protocol handler module interface",
     {"setup and teardown", fun test_start_stop/0}}.


%% Test definitions

test_start_stop() ->
    Specs = [proto_hdlr_up,
             proto_hdlr_handshake,
             proto_hdlr_down],
    ConnHdlrRef = self(),
    {ok, EvMgrRef} = event_queue:start_link(),
    EntTab = fs_ent_tab:create(),
    FsCtx = #fs_ctx{ ev_mgr_ref = EvMgrRef,
                     ent_tab    = EntTab },
    {ok, ProtoRef} = tfsp_proto:start_link(ConnHdlrRef, FsCtx),
    tfsp_proto:stop(ProtoRef),
    ?assertEqual(ok, event_queue:verify_strict(EvMgrRef, Specs)).
