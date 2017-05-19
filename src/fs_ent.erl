%%% Module for scanning and updating file system metadata entities.

-module(fs_ent).
-export([build/2]).

-include_lib("kernel/include/file.hrl").

-include("fs.hrl").

%% Defines

-define(HASH_BLOCK_SIZE, 65536).


%% Specs

-spec build(fs_path(), fs_path(), fs_ent()) -> {ok, fs_ent()} | {error, Reason :: any()}.


%% API

% Builds a new file system entity from the file/directory
% at the given root dir and path.
build(Root, Path) ->
    case path:read_link_info(Root, Path) of
        {ok, FileInfo} -> build(Root, Path, FileInfo);
        {error, Reason} -> {error, Reason}
    end.


%% Utilities

build(Root, Path, #file_info{ size = Size,
                              type = Type,
                              access = Access,
                              mtime = Mtime }) ->
    case Type of
        regular -> build_regular_ent(Root, Path, Size, Access, Mtime);
        directory -> build_directory_ent(Root, Path, Access, Mtime);
        _ -> {error, {invalid_type, Type}}
    end.

build_regular_ent(Root, Path, Size, Access, Mtime) ->
    Sha256 = build_sha256(Root, Path),
    {ok, #fs_ent{ path      = Path,
                  sha256    = Sha256,
                  size      = Size,
                  type      = regular,
                  access    = Access,
                  mtime     = Mtime,
                  deleted   = false }}.

build_directory_ent(_Root, Path, Access, Mtime) ->
    {ok, #fs_ent{ path      = Path,
                  type      = directory,
                  access    = Access,
                  mtime     = Mtime,
                  deleted   = false }}.

build_sha256(Root, Path) ->
    {ok, IoDevice} = path:open(Root, Path, [read, raw]),
    Sha256 = build_sha256_from_fd(IoDevice, crypto:hash_init(sha256)),
    ok = file:close(IoDevice),
    Sha256.

build_sha256_from_fd(IoDevice, Ctx) ->
    case file:read(IoDevice, ?HASH_BLOCK_SIZE) of
        eof ->
            crypto:hash_final(Ctx);
        {ok, Data} ->
            build_sha256_from_fd(IoDevice, crypto:hash_update(Ctx, Data))
    end.
