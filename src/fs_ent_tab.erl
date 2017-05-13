%%% Module for interfacing with the in-memory file system entity table.

-module(fs_ent_tab).
-export([create/0,
         delete/0,

         insert/1,
         find/1,
         remove/1,

         count/0]).

-include("fs_ent.hrl").


%% Specs

-spec create() -> atom().
-spec delete() -> ok.

-spec insert(fs_ent()) -> ok.
-spec find(file:path()) -> {ok, fs_ent()} | none.
-spec remove(file:path()) -> ok.

-spec count() -> non_neg_integer().


%% API 

% Creates the table.
create() ->
    ?MODULE = ets:new(?MODULE, [ordered_set,
                                public,
                                named_table,
                                {keypos, #fs_ent.path}]),
    ok.

% Deletes the table.
delete() ->
    true = ets:delete(?MODULE),
    ok.

% Inserts or updates a file entity in the table.
insert(Ent) ->
    true = ets:insert(?MODULE, Ent),
    ok.

% Finds a file entity from the table with the given path.
find(Path) ->
    case ets:lookup(?MODULE, Path) of
        [] -> none;
        [Ent] -> {ok, Ent}
    end.

remove(Path) ->
    true = ets:delete(?MODULE, Path),
    ok.

% Returns the number of entries in the table.
count() ->
    InfoList = ets:info(?MODULE),
    {size, Size} = proplists:lookup(size, InfoList),
    Size.
