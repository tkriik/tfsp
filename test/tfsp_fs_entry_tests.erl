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
    [{"file entry construction with small file", fun small_file_build/0},
     {"file entry construction with large file", fun large_file_build/0}].

%% Tests

small_file_build() ->
    file_build(?SMALL_FILE, <<>>, 1064, 1494221667).

large_file_build() ->
    file_build(?LARGE_FILE, <<>>, 286889, 1494221667).

file_build(Filename, Hash, Size, Mtime) ->
    Path = filename:join(?DATA_DIR, Filename),
    Entry = tfsp_fs_entry:build(Path),
    ?assertEqual(Path, Entry#fs_entry.path),
    ?assertEqual(Hash, Entry#fs_entry.hash),
    ?assertEqual(Size, Entry#fs_entry.size),
    ?assertEqual(regular, Entry#fs_entry.type),
    ?assertEqual(read_write, Entry#fs_entry.access),
    ?assertEqual(Mtime, Entry#fs_entry.mtime).
