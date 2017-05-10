%%% Tests for the file system entry scanner interface.

-module(tfsp_fs_scanner_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("fs_entry.hrl").

-define(SCAN_PATH, <<"test/data/">>).
-define(MODIFIED_FILE, <<"nested/LICENSE_COPY.txt">>).
-define(NEW_DIR, <<"new/">>).
-define(NEW_FILE, <<"new_file">>).

-define(TFSP_RE, "^[.].*.tfsp$").
-define(DAT_RE, "^.*.dat$").


%% Main tests

module_test_() ->
    {"Verifies that the file system entry scan routine "
     "works as expected.",
     [{"scanning existing files", scan_tests()},
      {"checking deleted files", deleted_tests()}
     ]
    }.

scan_tests() ->
    {"scanning existing files",
     {foreach,
      fun setup/0,
      fun cleanup/1,
      [{"once", fun scan_once/0},
       {"once with ignored regexp list", fun scan_once_ignore/0},
       {"twice", fun scan_twice/0},
       {"twice with 1 modified", fun scan_twice_modify/0},
       {"twice with 2 files created", fun scan_twice_create/0},
       {"twice with 2 files created and 1 modified", fun scan_twice_create_modify/0}
      ]
     }
    }.

deleted_tests() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"after initial scan", fun check_deleted_initial/0},
      {"with 1 deleted after initial scan", fun check_deleted_one/0},
      {"with 2 deleted after initial scan", fun check_deleted_one/0}
     ]
    }.


%% Test implementations

scan_once() ->
    ?assertEqual(7, tfsp_fs_scanner:scan(?SCAN_PATH, [])),
    ?assertEqual(7, tfsp_fs_table:count()).

scan_once_ignore() ->
    IgnoreRes = ignoreRes([?TFSP_RE, ?DAT_RE]),
    ?assertEqual(5, tfsp_fs_scanner:scan(?SCAN_PATH, IgnoreRes)),
    ?assertEqual(5, tfsp_fs_table:count()).


scan_twice() ->
    ?assertEqual(7, tfsp_fs_scanner:scan(?SCAN_PATH, [])),
    ?assertEqual(7, tfsp_fs_table:count()),
    ?assertEqual(0, tfsp_fs_scanner:scan(?SCAN_PATH, [])),
    ?assertEqual(7, tfsp_fs_table:count()).

scan_twice_modify() ->
    ?assertEqual(7, tfsp_fs_scanner:scan(?SCAN_PATH, [])),
    ?assertEqual(7, tfsp_fs_table:count()),
    set_mtime(?SCAN_PATH, ?MODIFIED_FILE, 2000000),
    ?assertEqual(1, tfsp_fs_scanner:scan(?SCAN_PATH, [])),
    ?assertEqual(7, tfsp_fs_table:count()).

scan_twice_create() ->
    ?assertEqual(7, tfsp_fs_scanner:scan(?SCAN_PATH, [])),
    ?assertEqual(7, tfsp_fs_table:count()),
    make_file(?SCAN_PATH, ?NEW_FILE),
    make_dir(?SCAN_PATH, ?NEW_DIR),
    ?assertEqual(2, tfsp_fs_scanner:scan(?SCAN_PATH, [])),
    ?assertEqual(9, tfsp_fs_table:count()).

scan_twice_create_modify() ->
    ?assertEqual(7, tfsp_fs_scanner:scan(?SCAN_PATH, [])),
    ?assertEqual(7, tfsp_fs_table:count()),
    set_mtime(?SCAN_PATH, ?MODIFIED_FILE, 2000000),
    make_file(?SCAN_PATH, ?NEW_FILE),
    make_dir(?SCAN_PATH, ?NEW_DIR),
    ?assertEqual(3, tfsp_fs_scanner:scan(?SCAN_PATH, [])),
    ?assertEqual(9, tfsp_fs_table:count()).

check_deleted_initial() ->
    make_file(?SCAN_PATH, ?NEW_FILE),
    tfsp_fs_scanner:scan(?SCAN_PATH, []),
    ?assertEqual(0, tfsp_fs_scanner:check_deleted()).

check_deleted_one() ->
    make_file(?SCAN_PATH, ?NEW_FILE),
    tfsp_fs_scanner:scan(?SCAN_PATH, []),
    ?assertEqual(0, tfsp_fs_scanner:check_deleted()),
    {ok, ExistingEntry} = find_entry(?SCAN_PATH, ?NEW_FILE),
    del_file(?SCAN_PATH, ?NEW_FILE),
    ?assertEqual(1, tfsp_fs_scanner:check_deleted()),
    {ok, DeletedEntry} = find_entry(?SCAN_PATH, ?NEW_FILE),
    ?assertEqual(ExistingEntry#fs_entry{ deleted = true }, DeletedEntry),
    ?assertEqual(0, tfsp_fs_scanner:check_deleted()).

check_deleted_two() ->
    make_file(?SCAN_PATH, ?NEW_FILE),
    make_file(?SCAN_PATH, ?NEW_DIR),
    tfsp_fs_scanner:scan(?SCAN_PATH, []),
    ?assertEqual(0, tfsp_fs_scanner:check_deleted()),
    {ok, ExistingFile} = find_entry(?SCAN_PATH, ?NEW_FILE),
    {ok, ExistingDir} = find_entry(?SCAN_PATH, ?NEW_DIR),
    del_file(?SCAN_PATH, ?NEW_FILE),
    del_dir(?SCAN_PATH, ?NEW_DIR),
    ?assertEqual(2, tfsp_fs_scanner:check_deleted()),
    {ok, DeletedFile} = find_entry(?SCAN_PATH, ?NEW_FILE),
    {ok, DeletedDir} = find_entry(?SCAN_PATH, ?NEW_DIR),
    ?assertEqual(ExistingFile#fs_entry{ deleted = true }, DeletedFile),
    ?assertEqual(ExistingDir#fs_entry{ deleted = true }, DeletedDir),
    ?assertEqual(0, tfsp_fs_scanner:check_deleted()).


%% Fixtures

setup() ->
    set_mtime(?SCAN_PATH, ?MODIFIED_FILE, 1000000),
    ok = tfsp_fs_table:create().

cleanup(_) ->
    del_file(?SCAN_PATH, ?NEW_FILE),
    del_dir(?SCAN_PATH, ?NEW_DIR),
    ok = tfsp_fs_table:delete().


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

find_entry(Dir, Filename) ->
    tfsp_fs_table:find(filename:join(Dir, Filename)).

set_mtime(Dir, Filename, Mtime) ->
    FileInfo = #file_info{ mtime = Mtime },
    ok = file:write_file_info(filename:join(Dir, Filename), FileInfo,
                              [{time, posix}]).
