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
    ok = fs_ent_tab:create().

cleanup(_) ->
    ok = fs_ent_tab:delete().


%% Tests

empty_table(_) ->
    {"empty table count",
     [?_assertEqual(0, fs_ent_tab:count())]}.

insert(_) ->
    {"insert succeeds",
     [?_assertEqual(ok, fs_ent_tab:insert(?ENT_A0))]}.

insert_one(_) ->
    ok = fs_ent_tab:insert(?ENT_A0),
    {"one insert",
     [?_assertEqual(fs_ent_tab:count(), 1)]}.

insert_same_twice(_) ->
    ok = fs_ent_tab:insert(?ENT_A0),
    ok = fs_ent_tab:insert(?ENT_A0),
    {"count after duplicate insertion",
     [?_assertEqual(1, fs_ent_tab:count())]}.

insert_different_twice(_) ->
    ok = fs_ent_tab:insert(?ENT_A0),
    ok = fs_ent_tab:insert(?ENT_B0),
    {"count after two different insertions",
     [?_assertEqual(2, fs_ent_tab:count())]}.

remove(_) ->
    ok = fs_ent_tab:insert(?ENT_A0),
    {"remove succeeds",
     [?_assertEqual(ok, fs_ent_tab:remove(<<"A">>))]}.

remove_one(_) ->
    ok = fs_ent_tab:insert(?ENT_A0),
    ok = fs_ent_tab:insert(?ENT_B0),
    ok = fs_ent_tab:remove(<<"A">>),
    {"count after one removal",
     [?_assertEqual(1, fs_ent_tab:count())]}.

find_nonexistent(_) ->
    {"finding non-existent entity",
     [?_assertEqual(none, fs_ent_tab:find(<<"A">>))]}.

find_new(_) ->
    ok = fs_ent_tab:insert(?ENT_A0),
    {"finding new entity",
     [?_assertEqual({ok, ?ENT_A0}, fs_ent_tab:find(<<"A">>))]}.

find_updated(_) ->
    ok = fs_ent_tab:insert(?ENT_A0),
    ok = fs_ent_tab:insert(?ENT_A1),
    {"finding updated entity",
     [?_assertEqual({ok, ?ENT_A1}, fs_ent_tab:find(<<"A">>))]}.

find_removed(_) ->
    ok = fs_ent_tab:insert(?ENT_A0),
    ok = fs_ent_tab:remove(<<"A">>),
    {"finding removed entity",
     [?_assertEqual(none, fs_ent_tab:find(<<"A">>))]}.
