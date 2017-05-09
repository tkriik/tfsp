%%% File system entry scanner worker module.
%%% Recursively scans directory trees and stores
%%% each entry in tfsp_fs_table. It also uses the
%%% table to avoid scanning already existing entries,
%%% unless their modification time has changed.

-module(tfsp_fs_scanner).

-export([start_link/1]).

-ifdef(TFSP_TEST).
-export([scan/2]).
-endif.

-include_lib("kernel/include/file.hrl").

-include("fs_entry.hrl").

%% Specs

-spec start_link([term()]) -> pid().

-spec init([term()]) -> none().
-spec loop(non_neg_integer(), [re:mp()]) -> none().

-spec scan(file:path(), [re:mp()]) -> non_neg_integer().
-spec scan_entry(file:path(), [re:mp()], non_neg_integer()) -> non_neg_integer().
-spec update_entry(file:path(), [re:mp()], fs_entry()) -> non_neg_integer().
-spec create_entry(file:path(), [re:mp()]) -> non_neg_integer().


%% API

% Spawns a new scanner worker with the given root path,
% interval in seconds, and a list of regexes for ignoring
% certain file name patterns.
start_link([Path, Interval, IgnoreRes]) ->
    spawn_link(?MODULE, init, [Path, Interval, IgnoreRes]).


%% Utilities

% Initialization before loop
init([Path, Interval, IgnoreRes]) ->
    ok = file:set_cwd(Path),
    loop(Interval, IgnoreRes).

% Main loop
loop(Interval, IgnoreRes) ->
    timer:send_after(Interval * 1000, again),
    scan(<<"./">>, IgnoreRes),
    receive
        again -> loop(Interval, IgnoreRes);
        Other -> error_logger:error_msg("Unknown message: ~p~n", [Other])
    end.

% Main scan routine. Returns the number of entries built.
scan(Path, IgnoreRes) ->
    {ok, Filenames} = file:list_dir(Path),
    FullPaths = lists:map(with_path(Path), Filenames),
    AllowedPaths = lists:filter(is_path_allowed_with(IgnoreRes), FullPaths),
    lists:foldl(fun(Filename, Acc) ->
                        scan_entry(Filename, IgnoreRes, Acc)
                end, 0, AllowedPaths).

with_path(Path) ->
    fun(Filename) ->
            ensure_binary_path(filename:join(Path, Filename))
    end.

ensure_binary_path(Path) ->
    if is_binary(Path) -> Path;
       is_list(Path) -> list_to_binary(Path)
    end.

is_path_allowed_with(IgnoreRes) ->
    fun(Path) -> is_path_allowed(Path, IgnoreRes) end.

is_path_allowed(_, []) ->
    true;
is_path_allowed(Path, [IgnoreRe | IgnoreRes]) ->
    case re:run(filename:basename(Path), IgnoreRe) of
        nomatch -> is_path_allowed(Path, IgnoreRes);
        {match, _} -> false
    end.


% Scans a new entry if it does not exist in the fs table.
% If it exists, only rescans if modification time is greater
% than stored. The accumulator stores the number of entries
% scanned, used for a more efficient traversal with fold in scan/1.
scan_entry(Filename, IgnoreRes, Acc) ->
    case tfsp_fs_table:find(Filename) of
        {ok, Entry} ->
            Acc + update_entry(Filename, IgnoreRes, Entry);
        none ->
            Acc + create_entry(Filename, IgnoreRes)
    end.

update_entry(Filename, IgnoreRes, #fs_entry{ type = Type, mtime = OldMtime }) ->
    {ok, #file_info{ mtime = CurMtime }} = file:read_link_info(Filename, [{time, posix}]),
    Acc = case OldMtime < CurMtime of
        true -> create_entry(Filename, []);
        false -> 0
    end,
    case Type of
        directory -> Acc + scan(Filename, IgnoreRes); % recurse if directory
        _ -> Acc
    end.

create_entry(Filename, IgnoreRes) ->
    case tfsp_fs_entry:build(Filename) of
        {ok, Entry} ->
            tfsp_fs_table:insert(Entry),
            case Entry#fs_entry.type of
                regular -> 1;
                directory -> 1 + scan(Filename, IgnoreRes) % recurse if directory
            end;
        {error, _Reason} ->
            % ignore files that we can't access for whatever reason
            % error_logger:error_msg("Not adding file ~s due to error: ~p~n", [Filename, Reason]),
            tfsp_fs_table:remove(Filename), % Delete old entry if it exists
            0
    end.
