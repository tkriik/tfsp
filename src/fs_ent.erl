%%% Module for scanning and updating file system metadata entities.

-module(fs_ent).
-export([build/1]).

-include_lib("kernel/include/file.hrl").

-include("fs_ent.hrl").

%% Defines

-define(HASH_BLOCK_SIZE, 65536).


%% Specs

-spec build(file:path()) -> {ok, fs_ent()} | {error, Reason :: any()}.


%% API

% Builds a new file system entity from the file/directory
% at the given path.
build(Path) ->
    case file:read_link_info(Path, [{time, posix}]) of
        {ok, FileInfo} -> build(Path, FileInfo);
        {error, Reason} -> {error, Reason}
    end.


%% Utilities

build(Path, #file_info{ size = Size,
                        type = Type,
                        access = Access,
                        mtime = Mtime }) ->
    case Type of
        regular -> build_regular_ent(Path, Size, Access, Mtime);
        directory -> build_directory_ent(Path, Access, Mtime);
        _ -> {error, {invalid_type, Type}}
    end.

build_regular_ent(Path, Size, Access, Mtime) ->
    Hash = build_hash(Path),
    {ok, #fs_ent{ path = Path,
                    hash = Hash,
                    size = Size,
                    type = regular,
                    access = Access,
                    mtime = Mtime,
                    deleted = false }}.

build_directory_ent(Path, Access, Mtime) ->
    {ok, #fs_ent{ path = Path,
                    type = directory,
                    access = Access,
                    mtime = Mtime,
                    deleted = false }}.

build_hash(Path) ->
    {ok, IoDevice} = file:open(Path, [read, raw]),
    Hash = build_hash(IoDevice, crypto:hash_init(sha256)),
    ok = file:close(IoDevice),
    Hash.

build_hash(IoDevice, Ctx) ->
    case file:read(IoDevice, ?HASH_BLOCK_SIZE) of
        eof ->
            crypto:hash_final(Ctx);
        {ok, Data} ->
            build_hash(IoDevice, crypto:hash_update(Ctx, Data))
    end.
