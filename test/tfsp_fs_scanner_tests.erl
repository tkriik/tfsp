%%% Tests for the file system entry scanner interface.

-module(tfsp_fs_scanner_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SCAN_PATH, <<"test/data/">>).
-define(MODIFIED_FILE, <<"nested/LICENSE_COPY.txt">>).
-define(NEW_DIR, <<"new/">>).
-define(NEW_FILE, <<"new_file">>).

%% Main tests

scan_test_() ->
    {"Verifies that the file system entry scan routine "
     "works as expected.",
     {"scanning existing files",
      {foreach,
       fun setup/0,
       fun cleanup/1,
       [{"once", fun scan_once/0},
        {"twice", fun scan_twice/0},
        {"twice with 1 modified", fun scan_twice_modify/0},
        {"twice with 2 files created", fun scan_twice_create/0},
        {"twice with 2 files created and 1 modified", fun scan_twice_create_modify/0}
       ]
      }
     }
    }.


%% Test implementations

scan_once() ->
    ?assertEqual(5, tfsp_fs_scanner:scan(?SCAN_PATH)),
    ?assertEqual(5, tfsp_fs_table:count()).

scan_twice() ->
    ?assertEqual(5, tfsp_fs_scanner:scan(?SCAN_PATH)),
    ?assertEqual(5, tfsp_fs_table:count()),
    ?assertEqual(0, tfsp_fs_scanner:scan(?SCAN_PATH)),
    ?assertEqual(5, tfsp_fs_table:count()).

scan_twice_modify() ->
    ?assertEqual(5, tfsp_fs_scanner:scan(?SCAN_PATH)),
    ?assertEqual(5, tfsp_fs_table:count()),
    set_mtime(?SCAN_PATH, ?MODIFIED_FILE, 2000000),
    ?assertEqual(1, tfsp_fs_scanner:scan(?SCAN_PATH)),
    ?assertEqual(5, tfsp_fs_table:count()).

scan_twice_create() ->
    ?assertEqual(5, tfsp_fs_scanner:scan(?SCAN_PATH)),
    ?assertEqual(5, tfsp_fs_table:count()),
    make_file(?SCAN_PATH, ?NEW_FILE),
    make_dir(?SCAN_PATH, ?NEW_DIR),
    ?assertEqual(2, tfsp_fs_scanner:scan(?SCAN_PATH)),
    ?assertEqual(7, tfsp_fs_table:count()).

scan_twice_create_modify() ->
    ?assertEqual(5, tfsp_fs_scanner:scan(?SCAN_PATH)),
    ?assertEqual(5, tfsp_fs_table:count()),
    set_mtime(?SCAN_PATH, ?MODIFIED_FILE, 2000000),
    make_file(?SCAN_PATH, ?NEW_FILE),
    make_dir(?SCAN_PATH, ?NEW_DIR),
    ?assertEqual(3, tfsp_fs_scanner:scan(?SCAN_PATH)),
    ?assertEqual(7, tfsp_fs_table:count()).

%% Fixtures

setup() ->
    set_mtime(?SCAN_PATH, ?MODIFIED_FILE, 1000000),
    ok = tfsp_fs_table:create().

cleanup(_) ->
    del_file(?SCAN_PATH, ?NEW_FILE),
    del_dir(?SCAN_PATH, ?NEW_DIR),
    ok = tfsp_fs_table:delete().


%% Utilities

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

set_mtime(Dir, Filename, Mtime) ->
    FileInfo = #file_info{ mtime = Mtime },
    ok = file:write_file_info(filename:join(Dir, Filename), FileInfo,
                              [{time, posix}]).
