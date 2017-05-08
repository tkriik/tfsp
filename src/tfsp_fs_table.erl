%%% Module for interfacing with the in-memory file system entry table.

-module(tfsp_fs_table).
-export([create/0,
         delete/0,

         insert/1,
         find/1,
         remove/1,

         count/0]).

-include("fs_entry.hrl").


%% Specs

-spec create() -> atom().
-spec delete() -> ok.

-spec insert(fs_entry()) -> ok.
-spec find(file:path()) -> {ok, fs_entry()} | none.
-spec remove(file:path()) -> ok.

-spec count() -> non_neg_integer().


%% API 

% Creates the table.
create() ->
    ?MODULE = ets:new(?MODULE, [ordered_set,
                                public,
                                named_table,
                                {keypos, #fs_entry.path}]),
    ok.

% Deletes the table.
delete() ->
    true = ets:delete(?MODULE),
    ok.

% Inserts or updates a file entry in the table.
insert(Entry) ->
    true = ets:insert(?MODULE, Entry),
    ok.

% Finds a file entry from the table with the given path.
find(Path) ->
    case ets:lookup(?MODULE, Path) of
        [] -> none;
        [Entry] -> {ok, Entry}
    end.

remove(Path) ->
    true = ets:delete(?MODULE, Path),
    ok.

% Returns the number of entries in the table.
count() ->
    InfoList = ets:info(?MODULE),
    {size, Size} = proplists:lookup(size, ets:info(?MODULE)),
    Size.
