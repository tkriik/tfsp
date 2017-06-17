%%%-------------------------------------------------------------------
%%% tfsp synchronization supervisor, initialized per root path.
%%%-------------------------------------------------------------------

-module(tfsp_sync_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%%% Constants

-define(SUP_NAME, ?MODULE).
-define(SCAN_INTERVAL, 60). % TODO: define elsewhere?
-define(CHILD_TIMEOUT, 1000).

%%% Specs

-spec start_link(Root :: string(), Specs :: [term()]) -> {ok, pid()}.

-spec init(Args :: [term()]) -> {ok, {SupFlags :: map(), ChildSpecs :: [map()]}}.

%%% API

%% Starts and links to a synchronization supervisor,
%% with one or more specifications per root path.
start_link(Root, Specs) ->
    supervisor:start_link(?MODULE, [Root, Specs]).

%%% Supervisor callbacks

init([Root, Specs]) ->
    SupFlags = #{ strategy  => one_for_one,
                  intensity => 0,
                  period    => 1 },
    ScannerSpec = scanner_child_spec(Root),
    SyncSpecs = sync_specs_to_child_specs(Root, Specs),
    ChildSpecs = lists:append([ScannerSpec], SyncSpecs),
    {ok, {SupFlags, ChildSpecs}}.

%%% Utilities

%% Constructs a scanner process supervisor child specification
%% with given root path.
scanner_child_spec(Root) ->
    #{ id       => {tfsp_scanner, Root, make_ref()},
       start    => {tfsp_scanner, start_link, [Root, ?SCAN_INTERVAL]},
       restart  => permanent,
       shutdown => ?CHILD_TIMEOUT,
       type     => worker }.

%% Transforms a list of synchronization specs to supervisor child specs.
sync_specs_to_child_specs(Root, Specs) ->
    lists:map(fun(Spec) ->
                      case Spec of
                          {connect, _, _} = ConnectSpec ->
                              connect_spec_to_child_spec(Root, ConnectSpec);
                          {serve, _, _} = ServeSpec ->
                              serve_spec_to_child_spec(Root, ServeSpec)
                      end
              end, Specs).

connect_spec_to_child_spec(Root, {connect, ConnectTransport, SyncOptions}) ->
    #{ id       => {tfsp_client, Root, make_ref()},
       start    => {tfsp_client, start_link, [Root, ConnectTransport, SyncOptions]},
       restart  => permanent,
       shutdown => ?CHILD_TIMEOUT,
       type     => worker,
       modules  => [tfsp_client] }.

serve_spec_to_child_spec(Root, {serve, ServeTransport, SyncOptions}) ->
    #{ id       => {tfsp_server, Root, make_ref()},
       start    => {tfsp_server, start_link, [Root, ServeTransport, SyncOptions]},
       restart  => permanent,
       shutdown => ?CHILD_TIMEOUT,
       type     => worker,
       modules  => [tfsp_server] }.
