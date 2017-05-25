%%% Module for interfacing with the in-memory file system entity table.

-module(fs_ent_tab).
-export([create/0,
         delete/1,
         insert/2,
         find/2,
         first/1,
         next/2,
         remove/2,
         count/1]).

-include("fs.hrl").


%% Specs

-type handle() :: {fs_ent_tab, ets:tid()}.

-export_type([handle/0]).

-spec create() -> handle().
-spec delete(handle()) -> ok.

-spec insert(handle(), fs_ent()) -> ok.
-spec find(handle(), fs_path()) -> {ok, fs_ent()} | none.
-spec remove(handle(), fs_path()) -> ok.

-spec first(handle()) -> {ok, fs_path()} | none.
-spec next(handle(), fs_path()) -> {ok, fs_path()} | none.

-spec count(handle()) -> non_neg_integer().


%% API 

% Creates a table.
create() ->
    Tid = ets:new(?MODULE, [ordered_set, protected, {keypos, #fs_ent.path}]),
    {fs_ent_tab, Tid}.

% Deletes a table.
delete({fs_ent_tab, Tid}) ->
    true = ets:delete(Tid),
    ok.

% Inserts or updates a file entity in a table.
insert({fs_ent_tab, Tid}, #fs_ent{ path = Path } = Ent) ->
    ok = path:ensure_normalized_path(Path),
    true = ets:insert(Tid, Ent),
    ok.

% Finds a file entity from the table with the given path.
find({fs_ent_tab, Tid}, Path) ->
    case ets:lookup(Tid, Path) of
        [] -> none;
        [Ent] -> {ok, Ent}
    end.

remove({fs_ent_tab, Tid}, Path) ->
    true = ets:delete(Tid, Path),
    ok.

first({fs_ent_tab, Tid}) ->
    case ets:first(Tid) of
        '$end_of_table' -> none;
        Path -> {ok, Path}
    end.

next({fs_ent_tab, Tid}, Path1) ->
    case ets:next(Tid, Path1) of
        '$end_of_table' -> none;
        Path2 -> {ok, Path2}
    end.

% Returns the number of entries in the table.
count({fs_ent_tab, Tid}) ->
    InfoList = ets:info(Tid),
    {size, Size} = proplists:lookup(size, InfoList),
    Size.
