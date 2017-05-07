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
    case file:read_file_info(Path) of
        {ok, FileInfo} -> build(Path, FileInfo);
        {error, Reason} -> {error, Reason}
    end.

%% Utilities

build(Path, FileInfo) ->
    #fs_entry{ path = Path,
               hash = <<>>,
               crc = 0,
               file_info = FileInfo }.
