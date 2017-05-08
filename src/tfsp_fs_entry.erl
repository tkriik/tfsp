%%% Module for scanning and updating file system metadata entries.

-module(tfsp_fs_entry).
-export([build/1]).

-include_lib("kernel/include/file.hrl").

-include("fs_entry.hrl").


%% Specs

-spec build(file:path()) -> {ok, fs_entry()} | {error, Reason :: any()}.


%% API

% Builds a new file system entry from the file/directory
% at the given path.
build(Path) ->
    case file:read_file_info(Path, [{time, posix}]) of
        {ok, FileInfo} -> build(Path, FileInfo);
        {error, Reason} -> {error, Reason}
    end.

%% Utilities

build(Path, #file_info{ size = Size,
                        type = Type,
                        access = Access,
                        mtime = Mtime }) ->
	case Type of
        regular -> build_regular_entry(Path, Size, Access, Mtime);
        directory -> build_directory_entry(Path, Access, Mtime)
    end.

build_regular_entry(Path, Size, Access, Mtime) ->
    #fs_entry{ path = Path,
               hash = <<>>, % TODO: compute SHA256
               size = Size,
               type = regular,
               access = Access,
               mtime = Mtime }.

build_directory_entry(Path, Access, Mtime) ->
    #fs_entry{ path = Path,
               hash = <<>>, % ignore hash
               size = 0, % ignore size
               type = directory,
               access = Access,
               mtime = Mtime }.
