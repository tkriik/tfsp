%%% File system entry scan and update tests

-module(tfsp_fs_entry_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("fs_entry.hrl").

%% Defines

-define(DATA_DIR, "test/data/").
-define(SMALL_FILE, "LICENSE.txt").
-define(LARGE_FILE, "pelican.jpg").


%% Main test

module_test_() ->
    [{"file entry construction with small file", fun with_small_file/0},
     {"file entry construction with large file", fun with_large_file/0},
     {"file entry construction with directory", fun with_directory/0}].

%% Tests

with_small_file() ->
    with_file(filename:join(?DATA_DIR, ?SMALL_FILE),
              <<>>,
              1064,
              regular,
              read_write,
              1494221667).

with_large_file() ->
    with_file(filename:join(?DATA_DIR, ?LARGE_FILE),
              <<>>,
              286889,
              regular,
              read_write,
              1494221667).

with_directory() ->
    with_file(?DATA_DIR,
              <<>>,
              0,
              directory,
              read_write,
              1494221667).

with_file(Path, Hash, Size, Type, Access, Mtime) ->
    Entry = tfsp_fs_entry:build(Path),
    ?assertEqual(Path, Entry#fs_entry.path),
    ?assertEqual(Hash, Entry#fs_entry.hash),
    ?assertEqual(Size, Entry#fs_entry.size),
    ?assertEqual(Type, Entry#fs_entry.type),
    ?assertEqual(Access, Entry#fs_entry.access),
    ?assertEqual(Mtime, Entry#fs_entry.mtime).
