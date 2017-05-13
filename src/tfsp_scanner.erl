%%% File system entity scanner worker module.
%%% Recursively scans directory trees and stores
%%% each entity in fs_ent_tab. It also uses the
%%% table to avoid scanning already existing entries,
%%% unless their modification time has changed.

-module(tfsp_scanner).

-export([start_link/1]).

-ifdef(TFSP_TEST).
-export([scan/2,
         check_deleted/0]).
-endif.

-include_lib("kernel/include/file.hrl").

-include("fs_ent.hrl").

%% Specs

-spec start_link([term()]) -> pid().

-spec init([term()]) -> none().
-spec loop(non_neg_integer(), [re:mp()]) -> none().

-spec scan(file:path(), [re:mp()]) -> non_neg_integer().
-spec scan_ent(file:path(), [re:mp()], non_neg_integer()) -> non_neg_integer().
-spec update_ent(file:path(), [re:mp()], fs_ent()) -> non_neg_integer().
-spec create_ent(file:path(), [re:mp()]) -> non_neg_integer().

-spec check_deleted() -> non_neg_integer().
-spec fold_delete(fs_ent(), non_neg_integer()) -> non_neg_integer().
-spec mark_deleted(fs_ent()) -> ok.


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
    _NumScanned = scan(<<"./">>, IgnoreRes),
    _NumDeleted = check_deleted(),
    receive
        again -> loop(Interval, IgnoreRes);
        Other -> error_logger:error_msg("Unknown message: ~p~n", [Other])
    end.


%% Scanning

% Main scan routine. Returns the number of entries built.
scan(Path, IgnoreRes) ->
    {ok, Filenames} = file:list_dir(Path),
    FullPaths = lists:map(with_path(Path), Filenames),
    AllowedPaths = lists:filter(is_path_allowed_with(IgnoreRes), FullPaths),
    lists:foldl(fun(Filename, Acc) ->
                        scan_ent(Filename, IgnoreRes, Acc)
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

% Scans a new entity if it does not exist in the fs table.
% If it exists, only rescans if modification time is greater
% than stored. The accumulator stores the number of entries
% scanned, used for a more efficient traversal with fold in scan/1.
scan_ent(Filename, IgnoreRes, Acc) ->
    case fs_ent_tab:find(Filename) of
        {ok, Ent} ->
            Acc + update_ent(Filename, IgnoreRes, Ent);
        none ->
            Acc + create_ent(Filename, IgnoreRes)
    end.

update_ent(Filename, IgnoreRes, #fs_ent{ type = Type, mtime = OldMtime }) ->
    {ok, #file_info{ mtime = CurMtime }} = file:read_link_info(Filename, [{time, posix}]),
    Acc = case OldMtime < CurMtime of
        true -> create_ent(Filename, []);
        false -> 0
    end,
    case Type of
        directory -> Acc + scan(Filename, IgnoreRes); % recurse if directory
        _ -> Acc
    end.

create_ent(Filename, IgnoreRes) ->
    case fs_ent:build(Filename) of
        {ok, Ent} ->
            ok = fs_ent_tab:insert(Ent),
            case Ent#fs_ent.type of
                regular -> 1;
                directory -> 1 + scan(Filename, IgnoreRes) % recurse if directory
            end;
        {error, _Reason} ->
            % ignore files that we can't access for whatever reason
            % error_logger:error_msg("Not adding file ~s due to error: ~p~n", [Filename, Reason]),
            fs_ent_tab:remove(Filename), % Delete old entity if it exists
            0
    end.


%% Deleted checking

% Iterates over all the entries in the fs table and checks those
% with their deleted flag not set whether they still exist.
% Those that don't get their 'deleted' flag set to true.
% Returns the number of entries marked deleted.
check_deleted() ->
    ets:foldl(fun fold_delete/2, 0, fs_ent_tab).

fold_delete(#fs_ent{ path = Path, deleted = Deleted } = Ent, NumDeleted) ->
    case Deleted of
        true -> NumDeleted;
        false -> case file:read_link_info(Path) of
                     {error, enoent} ->
                         mark_deleted(Ent),
                         NumDeleted + 1;
                     {error, enotdir} ->
                         mark_deleted(Ent),
                         NumDeleted + 1;
                     _ ->
                         NumDeleted
                 end
    end.

mark_deleted(Ent) ->
    ok = fs_ent_tab:insert(Ent#fs_ent{ deleted = true }).
