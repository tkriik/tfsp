%%%-------------------------------------------------------------------
%%% tfsp top level supervisor.
%%%-------------------------------------------------------------------

-module(tfsp_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%% Constants

-define(SUP_NAME, ?MODULE).
-define(CHILD_TIMEOUT, 1000).

%%% Specs

-spec start_link(Specs :: [term()]) -> {ok, pid()}.

-spec init(Args :: [term()]) -> {ok, {SupFlags :: map(), ChildSpecs :: [map()]}}.

%%% API

%% Starts and links to top-level supervisor. 'Specs' contains
%% entries from tfsp configuration file.
start_link(Specs) ->
    supervisor:start_link({local, ?SUP_NAME}, ?MODULE, [Specs]).

%%% Supervisor callbacks

init([Specs]) ->
    SupFlags = #{ strategy  => one_for_one,
                  intensity => 0,
                  period    => 1 },
    SpecsByRoot = group_specs_by_root(Specs),
    ChildSpecs = specs_by_root_to_child_specs(SpecsByRoot),
    {ok, {SupFlags, ChildSpecs}}.

%%% Utilities

%% Returns a list of configurations specs grouped by their
%% respective root paths, as a key-value list.
group_specs_by_root(Specs) ->
    maps:to_list(lists:foldl(fun assoc_spec_with_root/2, #{}, Specs)).

assoc_spec_with_root(Spec, SpecsByRoot) ->
    {_Root, _Spec} = case Spec of
        {connect, Root, ConnectTransport, SyncOptions} ->
            {Root, {connect, ConnectTransport, SyncOptions}};
        {serve, Root, ServeTransport, SyncOptions} ->
            {Root, {serve, ServeTransport, SyncOptions}}
    end,
    maps:update_with(_Root, append_with(_Spec), [_Spec], SpecsByRoot).

append_with(Elem) ->
    fun(Elems) ->
            lists:append(Elems, [Elem])
    end.

%% Transforms a root-specs -list to a list of supervisor
%% child specifications.
specs_by_root_to_child_specs(SpecsByRoot) ->
    lists:map(fun({Root, Specs}) ->
                      #{ id         => {tfsp_sync_sup, Root, make_ref()},
                         start      => {tfsp_sync_sup, start_link, [Root, Specs]},
                         restart    => permanent,
                         shutdown   => ?CHILD_TIMEOUT,
                         type       => supervisor,
                         modules    => [tfsp_sync_sup] }
              end, SpecsByRoot).
