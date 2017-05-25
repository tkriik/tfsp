%%% File system entity scanner worker module.
%%% Recursively scans directory trees and stores
%%% each entity in fs_ent_tab. It also uses the
%%% table to avoid scanning already existing entries,
%%% unless their modification time has changed.

-module(tfsp_scanner).

-export([start_link/3]).

-ifdef(TFSP_TEST).
-export([scan/3,
         check_deleted/1]).
-endif.

-include_lib("kernel/include/file.hrl").

-include("fs.hrl").

%% Specs

-spec start_link(fs_ctx(), non_neg_integer(), [re:mp()]) -> pid().

-spec init([term()]) -> none().
-spec loop(fs_ctx(), non_neg_integer(), [re:mp()]) -> none().

-spec scan(fs_ctx(), fs_path(), [re:mp()]) -> non_neg_integer().
-spec scan_ent(fs_ctx(), fs_path(), [re:mp()], non_neg_integer()) -> non_neg_integer().
-spec update_ent(fs_ctx(), fs_path(), [re:mp()], fs_ent()) -> non_neg_integer().
-spec create_ent(fs_ctx(), fs_path(), [re:mp()]) -> non_neg_integer().

-spec check_deleted(fs_ctx()) -> non_neg_integer().
-spec mark_deleted(fs_ent_tab(), fs_ent()) -> ok.


%% API

% Spawns a new scanner worker with the given root path,
% interval in seconds, and a list of regexes for ignoring
% certain file name patterns.
start_link(FsCtx, Interval, IgnoreRes) ->
    spawn_link(?MODULE, init, [FsCtx, Interval, IgnoreRes]).


%% Utilities

% Initialization before loop
init([FsCtx, Interval, IgnoreRes]) ->
    loop(FsCtx, Interval, IgnoreRes).

% Main loop
loop(FsCtx, Interval, IgnoreRes) ->
    timer:send_after(Interval * 1000, again),
    _NumScanned = scan(FsCtx, <<"">>, IgnoreRes),
    _NumDeleted = check_deleted(FsCtx),
    receive
        again -> loop(FsCtx, Interval, IgnoreRes);
        Other -> error_logger:error_msg("Unknown message: ~p~n", [Other]) % TODO: handle this better
    end.


%% Scanning

% Main scan routine. Returns the number of entries built.
scan(#fs_ctx{ root = Root } = FsCtx, Path, IgnoreRes) ->
    {ok, Filenames} = path:list_dir(Root, Path),
    FullPaths = lists:map(with_path(Path), Filenames),
    AllowedPaths = lists:filter(is_path_allowed_with(IgnoreRes), FullPaths),
    lists:foldl(fun(Filename, Acc) ->
                        scan_ent(FsCtx, Filename, IgnoreRes, Acc)
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
scan_ent(#fs_ctx{ ent_tab = EntTab } = FsCtx, Filename, IgnoreRes, Acc) ->
    case fs_ent_tab:find(EntTab, Filename) of
        {ok, Ent} ->
            Acc + update_ent(FsCtx, Filename, IgnoreRes, Ent);
        none ->
            Acc + create_ent(FsCtx, Filename, IgnoreRes)
    end.

update_ent(#fs_ctx{ root = Root } = FsCtx, Filename, IgnoreRes,
           #fs_ent{ type = Type,
                    mtime = OldMtime }) ->
    {ok, #file_info{ mtime = CurMtime }} = path:read_link_info(Root, Filename),
    Acc = case OldMtime < CurMtime of
        true -> create_ent(FsCtx, Filename, []);
        false -> 0
    end,
    case Type of
        directory -> Acc + scan(FsCtx, Filename, IgnoreRes); % recurse if directory
        _ -> Acc
    end.

create_ent(#fs_ctx{ ev_mgr_ref = EvMgrRef, root = Root, ent_tab = EntTab } = FsCtx, Filename, IgnoreRes) ->
    case fs_ent:build(Root, Filename) of
        {ok, Ent} ->
            ok = fs_ent_tab:insert(EntTab, Ent),
            ok = tfsp_event:notify_fs_ent_created(EvMgrRef, Ent),
            case Ent#fs_ent.type of
                regular -> 1;
                directory -> 1 + scan(FsCtx, Filename, IgnoreRes) % recurse if directory
            end;
        {error, _Reason} ->
            % ignore files that we can't access for whatever reason
            % error_logger:error_msg("Not adding file ~s due to error: ~p~n", [Filename, Reason]),
            fs_ent_tab:remove(EntTab, Filename), % Delete old entity if it exists
            0
    end.


%% Deleted checking

% Iterates over all the entries in the fs table and checks those
% with their deleted flag not set whether they still exist.
% Those that don't get their 'deleted' flag set to true.
% Returns the number of entries marked deleted.
check_deleted(#fs_ctx{ root = Root, ent_tab = {fs_ent_tab, Tid} } = FsCtx) -> % TODO: refactor
    ets:foldl(fun(#fs_ent{ path = Path, deleted = Deleted } = Ent, NumDeleted) ->
                      case Deleted of
                          true -> NumDeleted;
                          false -> case path:read_link_info(Root, Path) of
                                       {error, enoent} ->
                                           mark_deleted(FsCtx, Ent),
                                           NumDeleted + 1;
                                       {error, enotdir} ->
                                           mark_deleted(FsCtx, Ent),
                                           NumDeleted + 1;
                                       _ ->
                                           NumDeleted
                                   end
                      end
              end, 0, Tid).

mark_deleted(#fs_ctx{ ev_mgr_ref = EvMgrRef, ent_tab = EntTab }, Ent) ->
    DeletedEnt = Ent#fs_ent{ deleted = true },
    ok = fs_ent_tab:insert(EntTab, DeletedEnt),
    ok = tfsp_event:notify_fs_ent_deleted(EvMgrRef, DeletedEnt).
