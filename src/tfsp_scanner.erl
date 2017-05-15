%%% File system entity scanner worker module.
%%% Recursively scans directory trees and stores
%%% each entity in fs_ent_tab. It also uses the
%%% table to avoid scanning already existing entries,
%%% unless their modification time has changed.

-module(tfsp_scanner).

-export([start_link/4]).

-ifdef(TFSP_TEST).
-export([scan/3,
         check_deleted/1]).
-endif.

-include_lib("kernel/include/file.hrl").

-include("fs_ent.hrl").

%% Specs

-spec start_link(fs_ent_tab:handle(), file:path(), non_neg_integer(), [re:mp()]) -> pid().

-spec init([term()]) -> none().
-spec loop(fs_ent_tab:handle(), non_neg_integer(), [re:mp()]) -> none().

-spec scan(fs_ent_tab:handle(), file:path(), [re:mp()]) -> non_neg_integer().
-spec scan_ent(fs_ent_tab:handle(), file:path(), [re:mp()], non_neg_integer()) -> non_neg_integer().
-spec update_ent(fs_ent_tab:handle(), file:path(), [re:mp()], fs_ent()) -> non_neg_integer().
-spec create_ent(fs_ent_tab:handle(), file:path(), [re:mp()]) -> non_neg_integer().

-spec check_deleted(fs_ent_tab:handle()) -> non_neg_integer().
-spec mark_deleted(fs_ent_tab:handle(), fs_ent()) -> ok.


%% API

% Spawns a new scanner worker with the given root path,
% interval in seconds, and a list of regexes for ignoring
% certain file name patterns.
start_link(Table, Path, Interval, IgnoreRes) ->
    spawn_link(?MODULE, init, [Table, Path, Interval, IgnoreRes]).


%% Utilities

% Initialization before loop
init([Table, Path, Interval, IgnoreRes]) ->
    ok = file:set_cwd(Path),
    loop(Table, Interval, IgnoreRes).

% Main loop
loop(Table, Interval, IgnoreRes) ->
    timer:send_after(Interval * 1000, again),
    _NumScanned = scan(Table, <<"./">>, IgnoreRes),
    _NumDeleted = check_deleted(Table),
    receive
        again -> loop(Table, Interval, IgnoreRes);
        Other -> error_logger:error_msg("Unknown message: ~p~n", [Other]) % TODO: handle this better
    end.


%% Scanning

% Main scan routine. Returns the number of entries built.
scan(Table, Path, IgnoreRes) ->
    {ok, Filenames} = file:list_dir(Path),
    FullPaths = lists:map(with_path(Path), Filenames),
    AllowedPaths = lists:filter(is_path_allowed_with(IgnoreRes), FullPaths),
    lists:foldl(fun(Filename, Acc) ->
                        scan_ent(Table, Filename, IgnoreRes, Acc)
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
scan_ent(Table, Filename, IgnoreRes, Acc) ->
    case fs_ent_tab:find(Table, Filename) of
        {ok, Ent} ->
            Acc + update_ent(Table, Filename, IgnoreRes, Ent);
        none ->
            Acc + create_ent(Table, Filename, IgnoreRes)
    end.

update_ent(Table, Filename, IgnoreRes, #fs_ent{ type = Type,
                                                mtime = OldMtime }) ->
    {ok, #file_info{ mtime = CurMtime }} = file:read_link_info(Filename, [{time, posix}]),
    Acc = case OldMtime < CurMtime of
        true -> create_ent(Table, Filename, []);
        false -> 0
    end,
    case Type of
        directory -> Acc + scan(Table, Filename, IgnoreRes); % recurse if directory
        _ -> Acc
    end.

create_ent(Table, Filename, IgnoreRes) ->
    case fs_ent:build(Filename) of
        {ok, Ent} ->
            ok = fs_ent_tab:insert(Table, Ent),
            case Ent#fs_ent.type of
                regular -> 1;
                directory -> 1 + scan(Table, Filename, IgnoreRes) % recurse if directory
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
check_deleted({fs_ent_tab, Tid} = Table) -> % TODO: refactor
    ets:foldl(fun(#fs_ent{ path = Path, deleted = Deleted } = Ent, NumDeleted) ->
                      case Deleted of
                          true -> NumDeleted;
                          false -> case file:read_link_info(Path) of
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
