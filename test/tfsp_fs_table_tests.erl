%% Tests for the file system entry module interface.

-module(tfsp_fs_table_tests).

-include_lib("eunit/include/eunit.hrl").


%% Sample entries

-define(ENTRY_0, {tfsp_fs_entry, "A", 0 }).
-define(ENTRY_1, {tfsp_fs_entry, "A", 1 }).


%% Main test

module_test_() ->
    {"Verifies that the file system entry module interface "
     "works as expected.",
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [{"inserting new entry once", fun insert_once/0},
       {"inserting new entry twice", fun insert_twice/0},
       {"finding nonexistent entry", fun find_nonexistent/0},
       {"finding new entry", fun find_new/0},
       {"finding updated entry", fun find_updated/0},
       {"finding removed entry", fun find_removed/0}]
     }}.

%% Fixtures

setup() ->
    ok = tfsp_fs_table:new().

cleanup(_) ->
    ok = tfsp_fs_table:delete().


%% Tests

insert_once() ->
    ?assert(tfsp_fs_table:insert(?ENTRY_0) =:= ok).

insert_twice() ->
    ?assert(tfsp_fs_table:insert(?ENTRY_0) =:= ok),
    ?assert(tfsp_fs_table:insert(?ENTRY_1) =:= ok).

find_nonexistent() ->
    ?assert(tfsp_fs_table:find("A") =:= none).

find_new() ->
    tfsp_fs_table:insert(?ENTRY_0),
    ?assertEqual(tfsp_fs_table:find("A"), {ok, ?ENTRY_0}).

find_updated() ->
    tfsp_fs_table:insert(?ENTRY_0),
    tfsp_fs_table:insert(?ENTRY_1),
    ?assertEqual(tfsp_fs_table:find("A"), {ok, ?ENTRY_1}).

find_removed() ->
    tfsp_fs_table:insert(?ENTRY_0),
    tfsp_fs_table:remove("A"),
    ?assert(tfsp_fs_table:find("A") =:= none).
