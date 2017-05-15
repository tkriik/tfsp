%%% File system entity scan and update tests

-module(fs_ent_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("fs_ent.hrl").

%% Defines

-define(DATA_DIR, <<"test/data/scan">>).
-define(SMALL_FILE, <<"LICENSE.txt">>).
-define(LARGE_FILE, <<"pelican.jpg">>).
-define(NESTED_DIR, <<"nested">>).
-define(SYMLINK_FILE, <<"symlink">>).


%% Main test

module_test_() ->
    {"File entity construction",
     [{"with valid files",
       [{"small file", fun with_small_file/0},
        {"large file", fun with_large_file/0},
        {"directory", fun with_directory/0}]
      },
      {"with invalid files",
       [{"symlink", fun with_symlink/0},
        {"nonexistent", fun with_nonexistent/0}]
      }]
    }.


%% Tests

with_small_file() ->
    Path = ?SMALL_FILE,
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
    Path = ?LARGE_FILE,
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
    set_mtime(?NESTED_DIR, 1494221667),
    with_file(?NESTED_DIR, undefined, undefined, directory, read_write, 1494221667, false).

with_symlink() ->
    with_error(?SYMLINK_FILE, {invalid_type, symlink}).

with_nonexistent() ->
    with_error(<<"NONEXISTENT">>, enoent).

with_file(Path, Sha256, Size, Type, Access, Mtime, Deleted) ->
    Result = fs_ent:build(root(), Path),
    ?assertMatch({ok, _}, Result),
    {ok, Ent} = Result,
    ?assertEqual(Path, Ent#fs_ent.path),
    ?assertEqual(Sha256, Ent#fs_ent.sha256),
    ?assertEqual(Size, Ent#fs_ent.size),
    ?assertEqual(Type, Ent#fs_ent.type),
    ?assertEqual(Access, Ent#fs_ent.access),
    ?assertEqual(Mtime, Ent#fs_ent.mtime),
    ?assertEqual(Deleted, Ent#fs_ent.deleted).

with_error(Path, Reason) ->
    Result = fs_ent:build(root(), Path),
    ?assertMatch({error, _}, Result),
    ?assertEqual(element(2, Result), Reason).

%% Utils

root() ->
    path:normalize_root(?DATA_DIR).

set_mtime(Path, Mtime) ->
    FileInfo = #file_info{ mtime = Mtime },
    ok = path:write_file_info(root(), Path, FileInfo).
