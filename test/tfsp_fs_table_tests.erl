%%% Tests for the file system entry module interface.

-module(tfsp_fs_table_tests).

-include_lib("eunit/include/eunit.hrl").


%% Sample entries

-define(ENTRY_A0, {fs_entry, "A", 0 }).
-define(ENTRY_A1, {fs_entry, "A", 1 }).
-define(ENTRY_B0, {fs_entry, "B", 0 }).
-define(ENTRY_B1, {fs_entry, "B", 1 }).


%% Main test

module_test_() ->
    {"Verifies that the file system entry module interface "
     "works as expected.",
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [{"inserting new entry once", fun insert_once/0},
       {"inserting same entry twice", fun insert_same_twice/0},
       {"inserting different entry twice", fun insert_different_twice/0},
       {"inserting and removing entry", fun insert_remove/0},
       {"finding nonexistent entry", fun find_nonexistent/0},
       {"finding new entry", fun find_new/0},
       {"finding updated entry", fun find_updated/0},
       {"finding removed entry", fun find_removed/0}
      ]
     }
    }.

%% Fixtures

setup() ->
    ok = tfsp_fs_table:create().

cleanup(_) ->
    ok = tfsp_fs_table:delete().


%% Tests

insert_once() ->
    ?assertEqual(tfsp_fs_table:count(), 0),
    ?assert(tfsp_fs_table:insert(?ENTRY_A0) =:= ok),
    ?assertEqual(tfsp_fs_table:count(), 1).

insert_same_twice() ->
    ?assertEqual(tfsp_fs_table:count(), 0),
    ?assert(tfsp_fs_table:insert(?ENTRY_A0) =:= ok),
    ?assertEqual(tfsp_fs_table:count(), 1),
    ?assert(tfsp_fs_table:insert(?ENTRY_A1) =:= ok),
    ?assertEqual(tfsp_fs_table:count(), 1).

insert_different_twice() ->
    ?assertEqual(tfsp_fs_table:count(), 0),
    ?assert(tfsp_fs_table:insert(?ENTRY_A0) =:= ok),
    ?assertEqual(tfsp_fs_table:count(), 1),
    ?assert(tfsp_fs_table:insert(?ENTRY_B0) =:= ok),
    ?assertEqual(tfsp_fs_table:count(), 2).

insert_remove() ->
    ?assertEqual(tfsp_fs_table:count(), 0),
    ?assert(tfsp_fs_table:insert(?ENTRY_A0) =:= ok),
    ?assertEqual(tfsp_fs_table:count(), 1),
    ?assert(tfsp_fs_table:remove("A") =:= ok),
    ?assertEqual(tfsp_fs_table:count(), 0).

find_nonexistent() ->
    ?assert(tfsp_fs_table:find("A") =:= none).

find_new() ->
    tfsp_fs_table:insert(?ENTRY_A0),
    ?assertEqual(tfsp_fs_table:find("A"), {ok, ?ENTRY_A0}).

find_updated() ->
    tfsp_fs_table:insert(?ENTRY_A0),
    tfsp_fs_table:insert(?ENTRY_A1),
    ?assertEqual(tfsp_fs_table:find("A"), {ok, ?ENTRY_A1}).

find_removed() ->
    tfsp_fs_table:insert(?ENTRY_A0),
    tfsp_fs_table:remove("A"),
    ?assert(tfsp_fs_table:find("A") =:= none).
