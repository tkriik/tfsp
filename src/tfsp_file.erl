%%%-------------------------------------------------------------------
%%% tfsp file utilities
%%%
%%% Contains functions for normalizing both absolute and relative paths,
%%% and file library wrappers that take a root path and sub-path as
%%% parameters instead of a single path.
%%%-------------------------------------------------------------------

-module(tfsp_file).

-export([normalize_root/1,
         normalize_path/1,
         is_root_normalized/1,
         is_path_normalized/1,
         read_link_info/2,
         write_file_info/3,
         list_dir/2,
         open/3]).

-export_type([path/0]).

%% Specs

-type path() :: binary().

-spec normalize_root(Root :: string() | binary()) -> path().
-spec normalize_path(Path :: string() | binary()) -> path().

-spec is_root_normalized(Root :: any()) -> boolean().
-spec is_path_normalized(Path :: any()) -> boolean().

%%% API

%% Root and sub-path normalization functions

%% Normalizes a root path by removing and current or upper directory
%% references and by expanding a possible home path.
normalize_root(Root) ->
    BinRoot = path_to_binary(Root),
    Sanitized = without_components(BinRoot, [<<".">>, <<"..">>]),
    HomeExpanded = expand_home(Sanitized),
    HomeExpanded.

%% Normalizes a relative path by removing any root, current or
%% upper directory references.
normalize_path(Path) ->
    BinPath = path_to_binary(Path),
    Sanitized = without_components(BinPath, [<<"/">>, <<".">>, <<"..">>]),
    Sanitized.

is_root_normalized(Root) ->
    erlang:is_binary(Root) andalso Root =:= normalize_root(Root).

is_path_normalized(Path) ->
    erlang:is_binary(Path) andalso Path =:= normalize_path(Path).

%% Erlang file library wrappers

read_link_info(Root, Path) ->
    file:read_link_info(join(Root, Path), [{time, posix}]).

write_file_info(Root, Path, FileInfo) ->
    file:write_file_info(join(Root, Path), FileInfo, [{time, posix}]).

list_dir(Root, Path) ->
    file:list_dir(join(Root, Path)).

open(Root, Path, Modes) ->
    file:open(join(Root, Path), Modes).


%%% Utilities

%% Normalizes a file path to a suitable format.
%% Currently converts a path to a binary and removes
%% any root, current or upper directory references.
without_components(Path, IgnoredComps) ->
    Comps = filename:split(Path),
    AllowedComps = lists:filter(fun(Comp) ->
                                        case lists:member(Comp, IgnoredComps) of
                                            true -> false;
                                            false -> true
                                        end
                                end, Comps),
    case AllowedComps of
        [] -> <<"">>;
        Others -> filename:join(Others)
    end.

expand_home(Path) ->
    Comps = filename:split(Path),
    case Comps of
        [<<"~">> | Rest] ->
            filename:join([home() | Rest]);
        _ ->
            Path
    end.

home() ->
    {ok, [[Home | _]]} = init:get_argument(home),
    path_to_binary(Home).

path_to_binary(Path) ->
    if erlang:is_list(Path) -> erlang:list_to_binary(Path);
       erlang:is_binary(Path) -> Path
    end.

ensure_normalized_root(Root) ->
    case is_root_normalized(Root) of
        true -> ok;
        false -> throw({tfsp_file_root_not_normalized, Root})
    end.

ensure_normalized_path(Path) ->
    case is_path_normalized(Path) of
        true -> ok;
        false -> throw({tfsp_file_path_not_normalized, Path})
    end.

join(Root, Path) ->
    ok = ensure_normalized_root(Root),
    ok = ensure_normalized_path(Path),
    filename:join(Root, Path).
