%%%-------------------------------------------------------------------
%%% tfsp file system entity table module
%%%
%%% Exports functions for storing, querying and inserting
%%% file system entities.
%%%-------------------------------------------------------------------

-module(tfsp_ent_tab).

%% API
-export([create/0,
         insert/2,
         lookup/2]).

%%% Records

-record(tfsp_ent_tab_ref, { tid :: ets:tid() }).

%%% Specs

-type tfsp_path() :: tfsp_file:path().
-type tfsp_ent() :: tfsp_ent:tfsp_ent().
-type tfsp_ent_tab_ref() :: #tfsp_ent_tab_ref{}.

-spec create() -> EntTabRef :: tfsp_ent_tab_ref().
-spec insert(EntTabRef :: tfsp_ent_tab_ref(), tfsp_ent()) -> ok.
-spec lookup(EntTabRef :: tfsp_ent_tab_ref(), tfsp_path()) -> {ok, tfsp_ent()} | none.

%%% API

create() ->
    Tid = ets:new(tfsp_ent_tab, [ordered_set,
                                 public,
                                 compressed,
                                 {keypos, tfsp_ent:path_pos()},
                                 {read_concurrency, true}]),
    #tfsp_ent_tab_ref{ tid = Tid }.

insert(#tfsp_ent_tab_ref{ tid = Tid }, Ent) ->
    ets:insert(Tid, Ent),
    ok.

lookup(#tfsp_ent_tab_ref{ tid = Tid }, Path) ->
    case ets:lookup(Tid, Path) of
        [Ent] ->
            {ok, Ent};
        [] ->
            none
    end.
