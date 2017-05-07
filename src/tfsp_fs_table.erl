%%% Module for interfacing with the in-memory file system entry table.

-module(tfsp_fs_table).
-export([new/0,
         insert/1,
         find/1,
         remove/1,
         delete/0]).

-include("tfsp_fs_entry.hrl").


%% Specs

-spec new() -> atom().
-spec insert(tfsp_fs_entry()) -> ok.
-spec find(file:path()) -> {ok, tfsp_fs_entry()} | none.
-spec remove(file:path()) -> ok.
-spec delete() -> ok.


%% API 

% Creates the table.
new() ->
    ?MODULE = ets:new(?MODULE, [ordered_set,
                                public,
                                named_table,
                                {keypos, #tfsp_fs_entry.path}]),
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

% Deletes the table.
delete() ->
    true = ets:delete(?MODULE),
    ok.
