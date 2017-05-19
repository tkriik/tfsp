%%% Tests for the file system entity module interface.

-module(fs_ent_tab_tests).

-include_lib("eunit/include/eunit.hrl").

-include("fs.hrl").


%% Mock entities

-define(ENT_A0, #fs_ent{ path = <<"A">>, mtime = 0 }).
-define(ENT_A1, #fs_ent{ path = <<"A">>, mtime = 1 }).
-define(ENT_B0, #fs_ent{ path = <<"B">>, mtime = 0 }).
-define(ENT_B1, #fs_ent{ path = <<"B">>, mtime = 1 }).


%% Main test

module_test_() ->
    {"file system entity table interface",
     {foreach, local, fun setup/0, fun cleanup/1,
      [fun empty_table/1,
       fun insert/1,
       fun insert_one/1,
       fun insert_same_twice/1,
       fun insert_different_twice/1,
       fun remove/1,
       fun remove_one/1,
       fun find_nonexistent/1,
       fun find_new/1,
       fun find_updated/1,
       fun find_removed/1]}
    }.

%% Fixtures

setup() ->
    fs_ent_tab:create().

cleanup(Table) ->
    ok = fs_ent_tab:delete(Table).


%% Tests

empty_table(Table) ->
    {"empty table count",
     [?_assertEqual(0, fs_ent_tab:count(Table))]}.

insert(Table) ->
    {"insert succeeds",
     [?_assertEqual(ok, fs_ent_tab:insert(Table, ?ENT_A0))]}.

insert_one(Table) ->
    ok = fs_ent_tab:insert(Table, ?ENT_A0),
    {"one insert",
     [?_assertEqual(1, fs_ent_tab:count(Table))]}.

insert_same_twice(Table) ->
    ok = fs_ent_tab:insert(Table, ?ENT_A0),
    ok = fs_ent_tab:insert(Table, ?ENT_A0),
    {"count after duplicate insertion",
     [?_assertEqual(1, fs_ent_tab:count(Table))]}.

insert_different_twice(Table) ->
    ok = fs_ent_tab:insert(Table, ?ENT_A0),
    ok = fs_ent_tab:insert(Table, ?ENT_B0),
    {"count after two different insertions",
     [?_assertEqual(2, fs_ent_tab:count(Table))]}.

remove(Table) ->
    ok = fs_ent_tab:insert(Table, ?ENT_A0),
    {"remove succeeds",
     [?_assertEqual(ok, fs_ent_tab:remove(Table, <<"A">>))]}.

remove_one(Table) ->
    ok = fs_ent_tab:insert(Table, ?ENT_A0),
    ok = fs_ent_tab:insert(Table, ?ENT_B0),
    ok = fs_ent_tab:remove(Table, <<"A">>),
    {"count after one removal",
     [?_assertEqual(1, fs_ent_tab:count(Table))]}.

find_nonexistent(Table) ->
    {"finding non-existent entity",
     [?_assertEqual(none, fs_ent_tab:find(Table, <<"A">>))]}.

find_new(Table) ->
    ok = fs_ent_tab:insert(Table, ?ENT_A0),
    {"finding new entity",
     [?_assertEqual({ok, ?ENT_A0}, fs_ent_tab:find(Table, <<"A">>))]}.

find_updated(Table) ->
    ok = fs_ent_tab:insert(Table, ?ENT_A0),
    ok = fs_ent_tab:insert(Table, ?ENT_A1),
    {"finding updated entity",
     [?_assertEqual({ok, ?ENT_A1}, fs_ent_tab:find(Table, <<"A">>))]}.

find_removed(Table) ->
    ok = fs_ent_tab:insert(Table, ?ENT_A0),
    ok = fs_ent_tab:remove(Table, <<"A">>),
    {"finding removed entity",
     [?_assertEqual(none, fs_ent_tab:find(Table, <<"A">>))]}.
