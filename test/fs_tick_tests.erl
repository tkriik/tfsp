%%% Tests for the file system entity change (tick) announcer module
-module(fs_tick_tests).

-include_lib("eunit/include/eunit.hrl").


%% Main test

tick_test_() ->
    {"file system change announcer test", fun test_tick/0}.


%% Test definitions

test_tick() ->
    {ok, EvMgrRef} = tfsp_event:start_link(),
    ok = tfsp_event:add_fs_tick_sup_handler(EvMgrRef, self()),
    ok = tfsp_event:notify_fs_ent_created(EvMgrRef, stub_ent_one),
    ok = tfsp_event:notify_fs_ent_created(EvMgrRef, stub_ent_two),
    ok = tfsp_event:notify_misc(EvMgrRef, ignored_event),
    ok = tfsp_event:notify_fs_ent_deleted(EvMgrRef, stub_ent_one),
    ?assertEqual({fs_ent_created, stub_ent_one}, receive Msg -> Msg end),
    ?assertEqual({fs_ent_created, stub_ent_two}, receive Msg -> Msg end),
    ?assertEqual({fs_ent_deleted, stub_ent_one}, receive Msg -> Msg end).
