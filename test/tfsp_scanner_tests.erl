%%% Tests for the file system entity scanner interface.

-module(tfsp_scanner_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("fs.hrl").

-define(SCAN_PATH, <<"test/data/scan">>).
-define(MODIFIED_FILE, <<"nested/LICENSE_COPY.txt">>).
-define(NEW_DIR, <<"new">>).
-define(NEW_FILE, <<"new_file">>).

-define(TFSP_RE, "^[.].*.tfsp$").
-define(DAT_RE, "^.*.dat$").


%% Main tests

module_test_() ->
    {"file system entity scanner interface",
     [{"scanning existing entities", scan_tests()},
      {"checking deleted entities", deleted_tests()}]
    }.

scan_tests() ->
    {foreach, local, fun setup/0, fun cleanup/1,
     [fun scan_once/1,
      fun scan_once_ignore/1,
      fun scan_twice/1,
      fun scan_twice_modify/1,
      fun scan_twice_create/1,
      fun scan_twice_create_modify/1]}.

deleted_tests() ->
    {foreach, local, fun setup/0, fun cleanup/1,
     [fun check_none_deleted/1,
      fun check_one_deleted/1,
      fun check_two_deleted/1,
      fun check_deleted_ent/1]}.


%% Test implementations

scan_once(#fs_ctx{ ev_mgr_ref = EvMgrRef, ent_tab = EntTab } = FsCtx) ->
    Specs = lists:duplicate(7, fs_ent_created),
    {"with single scan",
     [?_assertEqual(7, tfsp_scanner:scan(FsCtx, <<"">>, [])),
      ?_assertEqual(7, fs_ent_tab:count(EntTab)),
      ?_assertEqual(ok, event_queue:verify_strict(EvMgrRef, Specs))]}.

scan_once_ignore(#fs_ctx { ev_mgr_ref = EvMgrRef, ent_tab = EntTab } = FsCtx) ->
    IgnoreRes = ignoreRes([?TFSP_RE, ?DAT_RE]),
    Specs = lists:duplicate(5, fs_ent_created),
    {"with ignored pattern list",
     [?_assertEqual(5, tfsp_scanner:scan(FsCtx, <<"">>, IgnoreRes)),
      ?_assertEqual(5, fs_ent_tab:count(EntTab)),
      ?_assertEqual(ok, event_queue:verify_strict(EvMgrRef, Specs))]}.

scan_twice(#fs_ctx { ent_tab = EntTab } = FsCtx) ->
    tfsp_scanner:scan(FsCtx, <<"">>, []),
    {"with duplicate scan",
     [?_assertEqual(0, tfsp_scanner:scan(FsCtx, <<"">>, [])),
      ?_assertEqual(7, fs_ent_tab:count(EntTab))]}.

scan_twice_modify(#fs_ctx{ ev_mgr_ref = EvMgrRef, root = Root, ent_tab = EntTab } = FsCtx) ->
    Specs = lists:duplicate(8, fs_ent_created),
    tfsp_scanner:scan(FsCtx, <<"">>, []),
    set_mtime(Root, ?MODIFIED_FILE, 2000000),
    {"with modification in between",
     [?_assertEqual(1, tfsp_scanner:scan(FsCtx, <<"">>, [])),
      ?_assertEqual(7, fs_ent_tab:count(EntTab)),
      ?_assertEqual(ok, event_queue:verify_strict(EvMgrRef, Specs))]}.

scan_twice_create(#fs_ctx{ ev_mgr_ref = EvMgrRef, root = Root, ent_tab = EntTab } = FsCtx) ->
    Specs = lists:duplicate(9, fs_ent_created),
    tfsp_scanner:scan(FsCtx, <<"">>, []),
    make_file(Root, ?NEW_FILE),
    make_dir(Root, ?NEW_DIR),
    {"with creation in between",
     [?_assertEqual(2, tfsp_scanner:scan(FsCtx, <<"">>, [])),
      ?_assertEqual(9, fs_ent_tab:count(EntTab)),
      ?_assertEqual(ok, event_queue:verify_strict(EvMgrRef, Specs))]}.

scan_twice_create_modify(#fs_ctx{ ev_mgr_ref = EvMgrRef, root = Root, ent_tab = EntTab } = FsCtx) ->
    Specs = lists:duplicate(9, fs_ent_created),
    tfsp_scanner:scan(FsCtx, <<"">>, []),
    set_mtime(Root, ?MODIFIED_FILE, 2000000),
    make_file(Root, ?NEW_FILE),
    make_dir(Root, ?NEW_DIR),
    {"with creation and modification in between",
     [?_assertEqual(3, tfsp_scanner:scan(FsCtx, <<"">>, [])),
      ?_assertEqual(9, fs_ent_tab:count(EntTab)),
      ?_assertEqual(ok, event_queue:verify_strict(EvMgrRef, Specs))]}.

check_none_deleted(FsCtx) ->
    tfsp_scanner:scan(FsCtx, <<"">>, []),
    {"with no deletion",
     [?_assertEqual(0, tfsp_scanner:check_deleted(FsCtx))]}.

check_one_deleted(#fs_ctx{ ev_mgr_ref = EvMgrRef, root = Root } = FsCtx) ->
    Specs = [fs_ent_deleted],
    make_file(Root, ?NEW_FILE),
    tfsp_scanner:scan(FsCtx, <<"">>, []),
    ok = event_queue:clear(EvMgrRef),
    del_file(Root, ?NEW_FILE),
    {"with one deleted",
     [?_assertEqual(1, tfsp_scanner:check_deleted(FsCtx)),
      ?_assertEqual(0, tfsp_scanner:check_deleted(FsCtx)),
      ?_assertEqual(ok, event_queue:verify_strict(EvMgrRef, Specs))]}.

check_two_deleted(#fs_ctx{ ev_mgr_ref = EvMgrRef, root = Root } = FsCtx) ->
    Specs = [fs_ent_deleted, fs_ent_deleted],
    make_file(Root, ?NEW_FILE),
    make_dir(Root, ?NEW_DIR),
    tfsp_scanner:scan(FsCtx, <<"">>, []),
    ok = event_queue:clear(EvMgrRef),
    del_file(Root, ?NEW_FILE),
    del_dir(Root, ?NEW_DIR),
    {"with two deleted",
     [?_assertEqual(2, tfsp_scanner:check_deleted(FsCtx)),
      ?_assertEqual(0, tfsp_scanner:check_deleted(FsCtx)),
      ?_assertEqual(ok, event_queue:verify_strict(EvMgrRef, Specs))]}.

check_deleted_ent(#fs_ctx{ root = Root, ent_tab = EntTab } = FsCtx) ->
    make_file(Root, ?NEW_FILE),
    tfsp_scanner:scan(FsCtx, <<"">>, []),
    {ok, ExistingEnt} = find_ent(EntTab, ?NEW_FILE),
    del_file(Root, ?NEW_FILE),
    tfsp_scanner:check_deleted(FsCtx),
    {ok, DeletedEnt} = find_ent(EntTab, ?NEW_FILE),
    {"whether deleted entity was marked deleted",
     [?_assertEqual(ExistingEnt#fs_ent{ deleted = true }, DeletedEnt)]}.


%% Fixtures

setup() ->
    {ok, EvMgrRef} = event_queue:start_link(),
    Root = root(),
    set_mtime(Root, ?MODIFIED_FILE, 1000000),
    del_file(Root, ?NEW_FILE),
    del_dir(Root, ?NEW_DIR),
    EntTab = fs_ent_tab:create(),
    #fs_ctx{ ev_mgr_ref     = EvMgrRef,
             root       = Root,
             ent_tab    = EntTab }.

cleanup(#fs_ctx{ ev_mgr_ref = EvMgrRef, root = Root, ent_tab = EntTab }) ->
    event_queue:stop(EvMgrRef),
    del_file(Root, ?NEW_FILE),
    del_dir(Root, ?NEW_DIR),
    ok = fs_ent_tab:delete(EntTab).


%% Utilities

root() ->
    path:normalize_root(?SCAN_PATH).

ignoreRes(Regexps) ->
    lists:map(fun(Regexp) -> {ok, Re} = re:compile(Regexp), Re end, Regexps).

make_file(Dir, Filename) ->
    {ok, IoDevice} = file:open(filename:join(Dir, Filename),
                               [read, write, exclusive]),
    ok = file:close(IoDevice).

make_dir(Dir, Filename) ->
    ok = file:make_dir(filename:join(Dir, Filename)).

del_file(Dir, Filename) ->
    file:delete(filename:join(Dir, Filename)).

del_dir(Dir, Filename) ->
    file:del_dir(filename:join(Dir, Filename)).

find_ent(EntTab, Filename) ->
    fs_ent_tab:find(EntTab, Filename).

set_mtime(Dir, Filename, Mtime) ->
    FileInfo = #file_info{ mtime = Mtime },
    ok = file:write_file_info(filename:join(Dir, Filename), FileInfo,
                              [{time, posix}]).
