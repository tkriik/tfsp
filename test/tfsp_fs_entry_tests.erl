%%% File system entry scan and update tests

-module(tfsp_fs_entry_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("fs_entry.hrl").

%% Defines

-define(DATA_DIR, <<"test/data/">>).
-define(SMALL_FILE, <<"LICENSE.txt">>).
-define(LARGE_FILE, <<"pelican.jpg">>).
-define(SYMLINK_FILE, <<"symlink">>).


%% Main test

module_test_() ->
    {"File entry construction",
     [{"with valid files",
       [{"small file", fun with_small_file/0},
        {"large file", fun with_large_file/0},
        {"directory", fun with_directory/0}
       ]
      },
      {"with invalid files",
       [{"symlink", fun with_symlink/0},
        {"nonexistent", fun with_nonexistent/0}
       ]
      }
     ]
    }.


%% Tests

with_small_file() ->
    Path = filename:join(?DATA_DIR, ?SMALL_FILE),
    set_mtime(Path, 1494221667),
    with_file(Path,
              <<131,41,250,6,40,66,164,44,119,157,178,254,125,150,165,27,169,
                229,25,100,150,77,81,190,146,114,124,253,109,144,120,60>>,
              1064,
              regular,
              read_write,
              1494221667,
              false).

with_large_file() ->
    Path = filename:join(?DATA_DIR, ?LARGE_FILE),
    set_mtime(Path, 1494221667),
    with_file(Path,
              <<216,164,148,172,100,134,39,211,7,162,233,96,64,24,48,209,1,235,
                236,194,166,85,60,251,252,88,141,117,59,153,209,14>>,
              286889,
              regular,
              read_write,
              1494221667,
              false).

with_directory() ->
    set_mtime(?DATA_DIR, 1494221667),
    with_file(?DATA_DIR, undefined, undefined, directory, read_write, 1494221667, false).

with_symlink() ->
    with_error(filename:join(?DATA_DIR, ?SYMLINK_FILE), {invalid_type, symlink}).

with_nonexistent() ->
    with_error(filename:join(?DATA_DIR, "NONEXISTENT"), enoent).

with_file(Path, Hash, Size, Type, Access, Mtime, Deleted) ->
    Result = tfsp_fs_entry:build(Path),
    ?assertMatch({ok, _}, Result),
    {ok, Entry} = Result,
    ?assertEqual(Path, Entry#fs_entry.path),
    ?assertEqual(Hash, Entry#fs_entry.hash),
    ?assertEqual(Size, Entry#fs_entry.size),
    ?assertEqual(Type, Entry#fs_entry.type),
    ?assertEqual(Access, Entry#fs_entry.access),
    ?assertEqual(Mtime, Entry#fs_entry.mtime),
    ?assertEqual(Deleted, Entry#fs_entry.deleted).

with_error(Path, Reason) ->
    Result = tfsp_fs_entry:build(Path),
    ?assertMatch({error, _}, Result),
    ?assertEqual(element(2, Result), Reason).

set_mtime(Path, Mtime) ->
    FileInfo = #file_info{ mtime = Mtime },
    ok = file:write_file_info(Path, FileInfo, [{time, posix}]).
