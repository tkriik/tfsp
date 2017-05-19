%%% File system entity scanner worker module.
%%% Recursively scans directory trees and stores
%%% each entity in fs_ent_tab. It also uses the
%%% table to avoid scanning already existing entries,
%%% unless their modification time has changed.

-module(tfsp_scanner).

-export([start_link/4]).

-ifdef(TFSP_TEST).
-export([scan/4,
         check_deleted/2]).
-endif.

-include_lib("kernel/include/file.hrl").

-include("fs.hrl").

%% Specs

-spec start_link(fs_tab(), fs_path(), non_neg_integer(), [re:mp()]) -> pid().

-spec init([term()]) -> none().
-spec loop(fs_tab(), fs_path(), non_neg_integer(), [re:mp()]) -> none().

-spec scan(fs_tab(), fs_path(), fs_path(), [re:mp()]) -> non_neg_integer().
-spec scan_ent(fs_tab(), fs_path(), fs_path(), [re:mp()], non_neg_integer()) -> non_neg_integer().
-spec update_ent(fs_tab(), fs_path(), fs_path(), [re:mp()], fs_ent()) -> non_neg_integer().
-spec create_ent(fs_tab(), fs_path(), fs_path(), [re:mp()]) -> non_neg_integer().

-spec check_deleted(fs_tab(), fs_path()) -> non_neg_integer().
-spec mark_deleted(fs_tab(), fs_ent()) -> ok.


%% API

% Spawns a new scanner worker with the given root path,
% interval in seconds, and a list of regexes for ignoring
% certain file name patterns.
start_link(Table, Root, Interval, IgnoreRes) ->
    spawn_link(?MODULE, init, [Table, Root, Interval, IgnoreRes]).


%% Utilities

% Initialization before loop
init([Table, Root, Interval, IgnoreRes]) ->
    loop(Table, Root, Interval, IgnoreRes).

% Main loop
loop(Table, Root, Interval, IgnoreRes) ->
    timer:send_after(Interval * 1000, again),
    _NumScanned = scan(Table, Root, <<"">>, IgnoreRes),
    _NumDeleted = check_deleted(Table, Root),
    receive
        again -> loop(Table, Root, Interval, IgnoreRes);
        Other -> error_logger:error_msg("Unknown message: ~p~n", [Other]) % TODO: handle this better
    end.


%% Scanning

% Main scan routine. Returns the number of entries built.
scan(Table, Root, Path, IgnoreRes) ->
    {ok, Filenames} = path:list_dir(Root, Path),
    FullPaths = lists:map(with_path(Path), Filenames),
    AllowedPaths = lists:filter(is_path_allowed_with(IgnoreRes), FullPaths),
    lists:foldl(fun(Filename, Acc) ->
                        scan_ent(Table, Root, Filename, IgnoreRes, Acc)
                end, 0, AllowedPaths).

with_path(Path) ->
    fun(Filename) ->
            path:normalize_path(filename:join(Path, Filename))
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
scan_ent(Table, Root, Filename, IgnoreRes, Acc) ->
    case fs_ent_tab:find(Table, Filename) of
        {ok, Ent} ->
            Acc + update_ent(Table, Root, Filename, IgnoreRes, Ent);
        none ->
            Acc + create_ent(Table, Root, Filename, IgnoreRes)
    end.

update_ent(Table, Root, Filename, IgnoreRes, #fs_ent{ type = Type,
                                                      mtime = OldMtime }) ->
    {ok, #file_info{ mtime = CurMtime }} = path:read_link_info(Root, Filename),
    Acc = case OldMtime < CurMtime of
        true -> create_ent(Table, Root, Filename, []);
        false -> 0
    end,
    case Type of
        directory -> Acc + scan(Table, Root, Filename, IgnoreRes); % recurse if directory
        _ -> Acc
    end.

create_ent(Table, Root, Filename, IgnoreRes) ->
    case fs_ent:build(Root, Filename) of
        {ok, Ent} ->
            ok = fs_ent_tab:insert(Table, Ent),
            case Ent#fs_ent.type of
                regular -> 1;
                directory -> 1 + scan(Table, Root, Filename, IgnoreRes) % recurse if directory
            end;
        {error, _Reason} ->
            % ignore files that we can't access for whatever reason
            % error_logger:error_msg("Not adding file ~s due to error: ~p~n", [Filename, Reason]),
            fs_ent_tab:remove(Table, Filename), % Delete old entity if it exists
            0
    end.


%% Deleted checking

% Iterates over all the entries in the fs table and checks those
% with their deleted flag not set whether they still exist.
% Those that don't get their 'deleted' flag set to true.
% Returns the number of entries marked deleted.
check_deleted({fs_ent_tab, Tid} = Table, Root) -> % TODO: refactor
    ets:foldl(fun(#fs_ent{ path = Path, deleted = Deleted } = Ent, NumDeleted) ->
                      case Deleted of
                          true -> NumDeleted;
                          false -> case path:read_link_info(Root, Path) of
                                       {error, enoent} ->
                                           mark_deleted(Table, Ent),
                                           NumDeleted + 1;
                                       {error, enotdir} ->
                                           mark_deleted(Table, Ent),
                                           NumDeleted + 1;
                                       _ ->
                                           NumDeleted
                                   end
                      end
              end, 0, Tid).

mark_deleted(Table, Ent) ->
    ok = fs_ent_tab:insert(Table, Ent#fs_ent{ deleted = true }).
