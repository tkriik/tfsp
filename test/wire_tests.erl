%%% Module tests for protocol event packing and unpacking routines.
-module(wire_tests).

-include_lib("eunit/include/eunit.hrl").

-include("fs_ent_defs.hrl").


%% Main tests

pack_unpack_test_() ->
    {"packing and unpacking protocol events",
     [fs_ent_info_tests()]}.


%% Test generators

fs_ent_info_tests() ->
    lists:map(fun mk_pack_unpack_fs_ent_info/1, fs_ent_samples()).

mk_pack_unpack_fs_ent_info(Ent) ->
    Out = wire:pack_fs_ent_info(Ent),
    In = wire:unpack_fs_ent_info(Out),
    {"with fs entity info", ?_assertEqual(Ent, In)}.


%% Utilities

fs_ent_samples() ->
    [?FS_ENT_LICENSE,
     ?FS_ENT_DAT,
     ?FS_ENT_PELICAN,
     ?FS_ENT_NESTED_DIR,
     ?FS_ENT_SCAN_DIR].
