%%%-------------------------------------------------------------------
%%% tfsp file system entity module
%%%
%%% Contains functions for scanning, constructing and accessing
%%% entities, which contain metadata about files and directories.
%%%-------------------------------------------------------------------

-module(tfsp_ent).

%% API
-export([build/2,
         build_maybe/3,
         type/1,
         mod_time/1,
         path_pos/0]).

-export_type([tfsp_ent/0]).

-include_lib("kernel/include/file.hrl").

%%% Constants

-define(HASH_BLOCK_SIZE, 65536).

%%% Records

-record(tfsp_ent, { % common attributes for both regular files and directories
                    path        :: binary(),
                    type        :: regular | directory,
                    access      :: read | read_write,
                    deleted     :: boolean(),
                    scan_time   :: non_neg_integer(),

                    % attributes specific for regular files
                    mod_time    :: non_neg_integer(),
                    size        :: non_neg_integer(),
                    sha256      :: binary() }).

%%% Specs

-type tfsp_path() :: tfsp_file:path().
-type tfsp_ent() :: #tfsp_ent{}.

-spec build(Root :: tfsp_path(), Path :: tfsp_path()) -> {ok, Ent :: tfsp_ent()} | {error, Reason :: term()}.
-spec build_maybe(Root      :: tfsp_path(),
                  Path      :: tfsp_path(),
                  ModTime   :: non_neg_integer()) ->
    {ok, Ent :: tfsp_ent()} | none | {error, Reason :: term()}.

%%% API

%% Builds a new file system entity from the file/directory
%% at the given path relative to root directory.
build(Root, Path) ->
    case tfsp_file:read_link_info(Root, Path) of
        {ok, FileInfo} ->
            build(Root, Path, FileInfo);
        {error, Reason} ->
            {error, Reason}
    end.

%% Builds a new file system entity if the
%% given modification time is older than
%% actual time.
build_maybe(Root, Path, ModTime) ->
    case tfsp_file:read_link_info(Root, Path) of
        {ok, FileInfo} ->
            case ModTime < FileInfo#file_info.mtime of
                true ->
                    build(Root, Path, FileInfo);
                false ->
                    none
            end;
        {error, Reason} ->
            {error, Reason}
    end.

type(#tfsp_ent{ type = Type }) ->
    Type.

mod_time(#tfsp_ent{ mod_time = ModTime }) ->
    ModTime.

path_pos() ->
    #tfsp_ent.path.

%%% Utilities

build(Root, Path, #file_info{ size = Size,
                              type = Type,
                              access = Access,
                              mtime = Mtime }) ->
    case Type of
        regular ->
            build_regular_ent(Root, Path, Size, Access, Mtime);
        directory ->
            build_directory_ent(Root, Path, Access);
        _ ->
            {error, {tfsp_ent_type_invalid, Type}}
    end.

build_regular_ent(Root, Path, Size, Access, Mtime) ->
    case build_sha256(Root, Path) of
        {ok, Sha256} ->
            {ok, #tfsp_ent{ path        = Path,
                            type        = regular,
                            access      = Access,
                            deleted     = false,
                            scan_time   = os:system_time(second),
                            mod_time    = Mtime,
                            size        = Size,
                            sha256      = Sha256 }};
        {error, Reason} ->
            {error, Reason}
    end.

build_directory_ent(_Root, Path, Access) ->
    {ok, #tfsp_ent{ path        = Path,
                    type        = directory,
                    access      = Access,
                    deleted     = false,
                    scan_time   = os:system_time(second) }}.

build_sha256(Root, Path) ->
    case tfsp_file:open(Root, Path, [read, raw]) of
        {ok, IoDevice} ->
            Result = build_sha256_from_fd(IoDevice, crypto:hash_init(sha256)),
            file:close(IoDevice),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

build_sha256_from_fd(IoDevice, Ctx) ->
    case file:read(IoDevice, ?HASH_BLOCK_SIZE) of
        {ok, Data} ->
            build_sha256_from_fd(IoDevice, crypto:hash_update(Ctx, Data));
        eof ->
            {ok, crypto:hash_final(Ctx)};
        {error, Reason} ->
            {error, Reason}
    end.
