%%%-------------------------------------------------------------------
%%% tfsp scanner server module
%%%
%%% Repeatedly scans root directories and stores new and modified
%%% file system entities to an entity table. Ignores calls or casts,
%%% except those sent by itself to launch a rescan at regular intervals.
%%%-------------------------------------------------------------------

-module(tfsp_scanner).

-behaviour(gen_server).

%% API
-export([start_link/3,
         scan/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/file.hrl").

%%% Records

-record(tfsp_scanner_state, { root          :: tfsp_path(),
                              ent_tab_ref   :: tfsp_ent_tab_ref(),
                              scan_interval :: non_neg_integer() }).

%%% Specs

-type tfsp_path() :: tfsp_file:path().
-type tfsp_ent_tab_ref() :: tfsp_ent_tab:tfsp_ent_tab_ref().
-type tfsp_scanner_state() :: #tfsp_scanner_state{}.

-spec start_link(Root           :: tfsp_path(),
                 EntTabRef      :: tfsp_ent_tab_ref(),
                 ScanInterval   :: non_neg_integer()) ->
    {ok, ServerRef :: pid()}.
-spec start_scan(ServerRef :: pid()) -> NumScanned :: non_neg_integer().
-spec start_single_scan(ServerRef :: pid(), Path :: tfsp_path()) -> NumScanned :: non_neg_integer().
-spec start_dir_scan(ServerRef :: pid(), Path :: tfsp_path()) -> NumScanned :: non_neg_integer().

-spec init(Args :: [term()]) -> {ok, tfsp_scanner_state()}.

%%% API

%% Spawns and links to a new scanner server with given root path
%% and scan interval.
start_link(Root, EntTabRef, ScanInterval) ->
    gen_server:start_link(?MODULE, [Root, EntTabRef, ScanInterval], []).

%% Casts a total scan launch message to scanner server.
start_scan(ServerRef) ->
    gen_server:cast(ServerRef, scan).

%% Casts a scan launch message for a single file to a scanner server.
start_single_scan(ServerRef, Path) ->
    gen_server:cast(ServerRef, {scan, Path}).

%% Commands a scanner server to scan all files under a directory.
start_dir_scan(ServerRef, Path) ->
    gen_server:cast(ServerRef, {scan_dir, Path}).

%%% gen_server callbacks

init([Root, EntTabRef, ScanInterval]) ->
    process_flag(trap_exit, true),
    start_scan(self()), % launch initial scan
    State = #tfsp_scanner_state{ root           = Root,
                                 ent_tab_ref    = EntTabRef,
                                 scan_interval  = ScanInterval },
    {ok, State}.

handle_call(Request, From, State) ->
    lager:warning("Unhandled call from ~p: ~p", [Request, From]),
    {noreply, State}.

handle_cast(scan, #tfsp_scanner_state{ root            = Root,
                                       scan_interval   = ScanInterval } = State) ->
    % Defer next scan message
    defer_start_scan(ScanInterval),
    lager:debug("Scanning modifications on ~s", [Root]),
    start_dir_scan(self(), <<"">>),
    {noreply, State};
handle_cast({scan, Path}, #tfsp_scanner_state{ root         = Root,
                                               ent_tab_ref  = EntTabRef } = State) ->
    lager:debug("~s: scanning ~s", [Root, Path]),
    case tfsp_ent_tab:lookup(EntTabRef, Path) of
        {ok, Ent} ->
            ModTime = tfsp_ent:mod_time(Ent),
            % Only rebuild if stored entity has older modification time
            case tfsp_ent:build_maybe(Root, Path, ModTime) of
                {ok, NewEnt} ->
                    ok = tfsp_ent_tab:insert(EntTabRef, NewEnt),
                    % Launch sub-scans if directory
                    case tfsp_ent:type(NewEnt) of
                        directory ->
                            ok = start_dir_scan(self(), Path);
                        _ ->
                            ok
                    end;
                {error, Reason} ->
                    lager:warning("~s: failed to build file system entity at ~s: ~p", [Root, Path, Reason])
            end;
        none ->
            case tfsp_ent:build(Root, Path) of
                {ok, Ent} ->
                    ok = tfsp_ent_tab:insert(EntTabRef, Ent),
                    % Launch sub-scans if directory
                    case tfsp_ent:type(Ent) of
                        directory ->
                            ok = start_dir_scan(self(), Path);
                        _ ->
                            ok
                    end;
                {error, Reason} ->
                    lager:warning("~s: failed to build file system entity at \"~s\": ~p", [Root, Path, Reason])
            end
    end,
    {noreply, State};
handle_cast({scan_dir, Path}, #tfsp_scanner_state{ root = Root } = State) ->
    case tfsp_file:list_dir(Root, Path) of
        {ok, Filenames} ->
            lists:map(fun(Filename) ->
                              _Filename = tfsp_file:normalize_path(Filename),
                              % prevent joining with empty prefix from creating absolute path
                              FullPath = case Path of 
                                             <<"">> -> _Filename;
                                             Dir    -> filename:join(Dir, _Filename)
                                         end,
                              start_single_scan(self(), FullPath)
                      end, Filenames);
        {error, Reason} ->
            lager:warning("~s: failed to list directory at \"~s\": ~p", [Root, Path, Reason])
    end,
    {noreply, State};
handle_cast(Request, State) ->
    lager:warning("Unhandled cast: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:warning("Unhandled termination: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Utilities

defer_start_scan(ScanInterval) ->
    {ok, _TRef} = timer:apply_after(ScanInterval * 1000, ?MODULE, scan, [self()]),
    ok.

scan(_Root) ->
    % TODO: implement
    0.

%%% Main scan routine. Returns the number of file system entities created.
%scan(#fs_ctx{ root = Root } = FsCtx, Path, IgnoreRes) ->
%    {ok, Filenames} = path:list_dir(Root, Path),
%    FullPaths = lists:map(with_path(Path), Filenames),
%    AllowedPaths = lists:filter(is_path_allowed_with(IgnoreRes), FullPaths),
%    lists:foldl(fun(Filename, Acc) ->
%                        scan_ent(FsCtx, Filename, IgnoreRes, Acc)
%                end, 0, AllowedPaths).
%
%with_path(Path) ->
%    fun(Filename) ->
%            path:normalize_path(filename:join(Path, Filename))
%    end.
%
%is_path_allowed_with(IgnoreRes) ->
%    fun(Path) -> is_path_allowed(Path, IgnoreRes) end.
%
%is_path_allowed(_, []) ->
%    true;
%is_path_allowed(Path, [IgnoreRe | IgnoreRes]) ->
%    case re:run(filename:basename(Path), IgnoreRe) of
%        nomatch -> is_path_allowed(Path, IgnoreRes);
%        {match, _} -> false
%    end.
%
%% Scans a new entity if it does not exist in the fs table.
%% If it exists, only rescans if modification time is greater
%% than stored. The accumulator stores the number of entries
%% scanned, used for a more efficient traversal with fold in scan/1.
%scan_ent(#fs_ctx{ ent_tab = EntTab } = FsCtx, Filename, IgnoreRes, Acc) ->
%    case fs_ent_tab:find(EntTab, Filename) of
%        {ok, Ent} ->
%            Acc + update_ent(FsCtx, Filename, IgnoreRes, Ent);
%        none ->
%            Acc + create_ent(FsCtx, Filename, IgnoreRes)
%    end.
%
%update_ent(#fs_ctx{ root = Root } = FsCtx, Filename, IgnoreRes,
%           #fs_ent{ type = Type,
%                    mtime = OldMtime }) ->
%    {ok, #file_info{ mtime = CurMtime }} = path:read_link_info(Root, Filename),
%    Acc = case OldMtime < CurMtime of
%        true -> create_ent(FsCtx, Filename, []);
%        false -> 0
%    end,
%    case Type of
%        directory -> Acc + scan(FsCtx, Filename, IgnoreRes); % recurse if directory
%        _ -> Acc
%    end.
%
%create_ent(#fs_ctx{ ev_mgr_ref = EvMgrRef, root = Root, ent_tab = EntTab } = FsCtx, Filename, IgnoreRes) ->
%    case fs_ent:build(Root, Filename) of
%        {ok, Ent} ->
%            ok = fs_ent_tab:insert(EntTab, Ent),
%            ok = tfsp_event:notify_fs_ent_created(EvMgrRef, Ent),
%            case Ent#fs_ent.type of
%                regular -> 1;
%                directory -> 1 + scan(FsCtx, Filename, IgnoreRes) % recurse if directory
%            end;
%        {error, _Reason} ->
%            % ignore files that we can't access for whatever reason
%            % error_logger:error_msg("Not adding file ~s due to error: ~p~n", [Filename, Reason]),
%            fs_ent_tab:remove(EntTab, Filename), % Delete old entity if it exists
%            0
%    end.
%
%
%%% Deleted checking
%
%% Iterates over all the entries in the fs table and checks those
%% with their deleted flag not set whether they still exist.
%% Those that don't get their 'deleted' flag set to true.
%% Returns the number of entries marked deleted.
%check_deleted(#fs_ctx{ root = Root, ent_tab = {fs_ent_tab, Tid} } = FsCtx) -> % TODO: refactor
%    ets:foldl(fun(#fs_ent{ path = Path, deleted = Deleted } = Ent, NumDeleted) ->
%                      case Deleted of
%                          true -> NumDeleted;
%                          false -> case path:read_link_info(Root, Path) of
%                                       {error, enoent} ->
%                                           mark_deleted(FsCtx, Ent),
%                                           NumDeleted + 1;
%                                       {error, enotdir} ->
%                                           mark_deleted(FsCtx, Ent),
%                                           NumDeleted + 1;
%                                       _ ->
%                                           NumDeleted
%                                   end
%                      end
%              end, 0, Tid).
%
%mark_deleted(#fs_ctx{ ev_mgr_ref = EvMgrRef, ent_tab = EntTab }, Ent) ->
%    DeletedEnt = Ent#fs_ent{ deleted = true },
%    ok = fs_ent_tab:insert(EntTab, DeletedEnt),
%    ok = tfsp_event:notify_fs_ent_deleted(EvMgrRef, DeletedEnt).
