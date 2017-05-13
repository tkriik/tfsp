%%% Tests for the file system entity scanner interface.

-module(tfsp_scanner_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("fs_ent.hrl").

-define(SCAN_PATH, <<"test/data/scan/">>).
-define(MODIFIED_FILE, <<"nested/LICENSE_COPY.txt">>).
-define(NEW_DIR, <<"new/">>).
-define(NEW_FILE, <<"new_file">>).

-define(TFSP_RE, "^[.].*.tfsp$").
-define(DAT_RE, "^.*.dat$").


%% Main tests

module_test_() ->
    {"file system entity scanner interface",
     [{"scanning existing entities", scan_tests()},
      {"checking deleted entities", deleted_tests()}]
    }.

scan_tests() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun scan_once/1,
      fun scan_once_ignore/1,
      fun scan_twice/1,
      fun scan_twice_modify/1,
      fun scan_twice_create/1,
      fun scan_twice_create_modify/1]}.

deleted_tests() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun check_none_deleted/1,
      fun check_one_deleted/1,
      fun check_two_deleted/1,
      fun check_deleted_ent/1]}.


%% Test implementations

scan_once(_) ->
    {"with single scan",
     [?_assertEqual(7, tfsp_scanner:scan(?SCAN_PATH, [])),
      ?_assertEqual(7, fs_ent_tab:count())]}.

scan_once_ignore(_) ->
    IgnoreRes = ignoreRes([?TFSP_RE, ?DAT_RE]),
    {"with ignored pattern list",
     [?_assertEqual(5, tfsp_scanner:scan(?SCAN_PATH, IgnoreRes)),
      ?_assertEqual(5, fs_ent_tab:count())]}.

scan_twice(_) ->
    tfsp_scanner:scan(?SCAN_PATH, []),
    {"with duplicate scan",
     [?_assertEqual(0, tfsp_scanner:scan(?SCAN_PATH, [])),
      ?_assertEqual(7, fs_ent_tab:count())]}.

scan_twice_modify(_) ->
    tfsp_scanner:scan(?SCAN_PATH, []),
    set_mtime(?SCAN_PATH, ?MODIFIED_FILE, 2000000),
    {"with modification in between",
     [?_assertEqual(1, tfsp_scanner:scan(?SCAN_PATH, [])),
      ?_assertEqual(7, fs_ent_tab:count())]}.

scan_twice_create(_) ->
    tfsp_scanner:scan(?SCAN_PATH, []),
    make_file(?SCAN_PATH, ?NEW_FILE),
    make_dir(?SCAN_PATH, ?NEW_DIR),
    {"with creation in between",
     [?_assertEqual(2, tfsp_scanner:scan(?SCAN_PATH, [])),
      ?_assertEqual(9, fs_ent_tab:count())]}.

scan_twice_create_modify(_) ->
    tfsp_scanner:scan(?SCAN_PATH, []),
    set_mtime(?SCAN_PATH, ?MODIFIED_FILE, 2000000),
    make_file(?SCAN_PATH, ?NEW_FILE),
    make_dir(?SCAN_PATH, ?NEW_DIR),
    {"with creation and modification in between",
     [?_assertEqual(3, tfsp_scanner:scan(?SCAN_PATH, [])),
      ?_assertEqual(9, fs_ent_tab:count())]}.

check_none_deleted(_) ->
    tfsp_scanner:scan(?SCAN_PATH, []),
    {"with no deletion",
     [?_assertEqual(0, tfsp_scanner:check_deleted())]}.

check_one_deleted(_) ->
    make_file(?SCAN_PATH, ?NEW_FILE),
    tfsp_scanner:scan(?SCAN_PATH, []),
    del_file(?SCAN_PATH, ?NEW_FILE),
    {"with one deleted",
     [?_assertEqual(1, tfsp_scanner:check_deleted()),
      ?_assertEqual(0, tfsp_scanner:check_deleted())]}.

check_two_deleted(_) ->
    make_file(?SCAN_PATH, ?NEW_FILE),
    make_dir(?SCAN_PATH, ?NEW_DIR),
    tfsp_scanner:scan(?SCAN_PATH, []),
    del_file(?SCAN_PATH, ?NEW_FILE),
    del_dir(?SCAN_PATH, ?NEW_DIR),
    {"with two deleted",
     [?_assertEqual(2, tfsp_scanner:check_deleted()),
      ?_assertEqual(0, tfsp_scanner:check_deleted())]}.

check_deleted_ent(_) ->
    make_file(?SCAN_PATH, ?NEW_FILE),
    tfsp_scanner:scan(?SCAN_PATH, []),
    {ok, ExistingEnt} = find_ent(?SCAN_PATH, ?NEW_FILE),
    del_file(?SCAN_PATH, ?NEW_FILE),
    tfsp_scanner:check_deleted(),
    {ok, DeletedEnt} = find_ent(?SCAN_PATH, ?NEW_FILE),
    {"whether deleted entity was marked deleted",
     [?_assertEqual(ExistingEnt#fs_ent{ deleted = true }, DeletedEnt)]}.


%% Fixtures

setup() ->
    set_mtime(?SCAN_PATH, ?MODIFIED_FILE, 1000000),
    del_file(?SCAN_PATH, ?NEW_FILE),
    del_dir(?SCAN_PATH, ?NEW_DIR),
    ok = fs_ent_tab:create().

cleanup(_) ->
    del_file(?SCAN_PATH, ?NEW_FILE),
    del_dir(?SCAN_PATH, ?NEW_DIR),
    ok = fs_ent_tab:delete().


%% Utilities

ignoreRes(Regexps) ->
    lists:map(fun(Regexp) -> {ok, Re} = re:compile(Regexp), Re end, Regexps).

make_file(Dir, Filename) ->
    {ok, IoDevice} = file:open(filename:join(Dir, Filename),
                               [read, write, exclusive]),
    ok = file:close(IoDevice).

make_dir(Dir, Filename) ->
    ok = file:make_dir(filename:join(Dir, Filename)).

del_file(Dir, Filename) ->
    file:delete(filename:join(Dir, Filename)).

del_dir(Dir, Filename) ->
    file:del_dir(filename:join(Dir, Filename)).

find_ent(Dir, Filename) ->
    fs_ent_tab:find(filename:join(Dir, Filename)).

set_mtime(Dir, Filename, Mtime) ->
    FileInfo = #file_info{ mtime = Mtime },
    ok = file:write_file_info(filename:join(Dir, Filename), FileInfo,
                              [{time, posix}]).
