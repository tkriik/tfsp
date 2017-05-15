% File utilities, mainly root-dir-pair -aware wrappers of regular
% file-module functions.
-module(path).
-export([normalize_root/1,
         normalize_path/1,
         ensure_normalized_root/1,
         ensure_normalized_path/1,
         read_link_info/2,
         write_file_info/3,
         list_dir/2,
         open/3]).


%% Specs

-type path() :: binary().

-spec normalize(path(), boolean()) -> path().
-spec ensure_normalized_root(path()) -> ok.
-spec ensure_normalized_path(path()) -> ok.


%% API

normalize_root(Root) ->
    normalize(filename:absname(Root), false).

normalize_path(Path) ->
    normalize(Path, true).

ensure_normalized_root(Root) ->
    ok = ensure_normalized(Root, false).

ensure_normalized_path(Path) ->
    ok = ensure_normalized(Path, true).

read_link_info(Root, Path) ->
    file:read_link_info(join(Root, Path), [{time, posix}]).

write_file_info(Root, Path, FileInfo) ->
    file:write_file_info(join(Root, Path), FileInfo, [{time, posix}]).

list_dir(Root, Path) ->
    file:list_dir(join(Root, Path)).

open(Root, Path, Modes) ->
    file:open(join(Root, Path), Modes).


%% Utilities

% Normalizes a file path to a suitable format.
% Currently converts a path to a binary and removes
% any root, current or upper directory references.
normalize(Path, RemoveSlash) ->
    BinPath = if erlang:is_list(Path) -> erlang:list_to_binary(Path);
                 erlang:is_binary(Path) -> Path
              end,
    Comps = filename:split(BinPath),
    ValidComps = lists:filter(fun(Comp) ->
                                      Comp =/= <<".">> andalso
                                      Comp =/= <<"..">> andalso
                                      case RemoveSlash of
                                          true -> Comp =/= <<"/">>;
                                          false -> true
                                      end
                              end, Comps),
    case ValidComps of
        [] -> <<"">>;
        _ValidComps -> filename:join(_ValidComps)
    end.

join(Root, Path) ->
    ok = ensure_normalized_root(Root),
    ok = ensure_normalized_path(Path),
    filename:join(Root, Path).

ensure_normalized(Path, RemoveSlash) ->
    case Path =:= normalize(Path, RemoveSlash) of
        true -> ok;
        false -> throw({path_not_normalized, Path})
    end.
