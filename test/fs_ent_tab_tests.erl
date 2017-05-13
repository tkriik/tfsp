%%% Tests for the file system entity module interface.

-module(fs_ent_tab_tests).

-include_lib("eunit/include/eunit.hrl").


%% Mock entities

-define(ENT_A0, {fs_ent, <<"A">>, 0 }).
-define(ENT_A1, {fs_ent, <<"A">>, 1 }).
-define(ENT_B0, {fs_ent, <<"B">>, 0 }).
-define(ENT_B1, {fs_ent, <<"B">>, 1 }).


%% Main test

module_test_() ->
    {"Verifies that the file system entity module interface "
     "works as expected.",
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [{"inserting new entity once", fun insert_once/0},
       {"inserting same entity twice", fun insert_same_twice/0},
       {"inserting different entity twice", fun insert_different_twice/0},
       {"inserting and removing entity", fun insert_remove/0},
       {"finding nonexistent entity", fun find_nonexistent/0},
       {"finding new entity", fun find_new/0},
       {"finding updated entity", fun find_updated/0},
       {"finding removed entity", fun find_removed/0}
      ]
     }
    }.

%% Fixtures

setup() ->
    ok = fs_ent_tab:create().

cleanup(_) ->
    ok = fs_ent_tab:delete().


%% Tests

insert_once() ->
    ?assertEqual(fs_ent_tab:count(), 0),
    ?assert(fs_ent_tab:insert(?ENT_A0) =:= ok),
    ?assertEqual(fs_ent_tab:count(), 1).

insert_same_twice() ->
    ?assertEqual(fs_ent_tab:count(), 0),
    ?assert(fs_ent_tab:insert(?ENT_A0) =:= ok),
    ?assertEqual(fs_ent_tab:count(), 1),
    ?assert(fs_ent_tab:insert(?ENT_A1) =:= ok),
    ?assertEqual(fs_ent_tab:count(), 1).

insert_different_twice() ->
    ?assertEqual(fs_ent_tab:count(), 0),
    ?assert(fs_ent_tab:insert(?ENT_A0) =:= ok),
    ?assertEqual(fs_ent_tab:count(), 1),
    ?assert(fs_ent_tab:insert(?ENT_B0) =:= ok),
    ?assertEqual(fs_ent_tab:count(), 2).

insert_remove() ->
    ?assertEqual(fs_ent_tab:count(), 0),
    ?assert(fs_ent_tab:insert(?ENT_A0) =:= ok),
    ?assertEqual(fs_ent_tab:count(), 1),
    ?assert(fs_ent_tab:remove(<<"A">>) =:= ok),
    ?assertEqual(fs_ent_tab:count(), 0).

find_nonexistent() ->
    ?assert(fs_ent_tab:find(<<"A">>) =:= none).

find_new() ->
    fs_ent_tab:insert(?ENT_A0),
    ?assertEqual(fs_ent_tab:find(<<"A">>), {ok, ?ENT_A0}).

find_updated() ->
    fs_ent_tab:insert(?ENT_A0),
    fs_ent_tab:insert(?ENT_A1),
    ?assertEqual(fs_ent_tab:find(<<"A">>), {ok, ?ENT_A1}).

find_removed() ->
    fs_ent_tab:insert(?ENT_A0),
    fs_ent_tab:remove(<<"A">>),
    ?assert(fs_ent_tab:find(<<"A">>) =:= none).
