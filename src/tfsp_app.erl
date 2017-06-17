%%%-------------------------------------------------------------------
%%% tfsp public API
%%%-------------------------------------------------------------------

-module(tfsp_app).

-behaviour(application).

%% API
-export([start/0,
         start/1]).

%% Application callbacks
-export([start/2,
         stop/1]).

%%% Constants

-define(APP_NAME, ?MODULE).
-define(DEFAULT_CONFIG_PATH, <<".config/tfsp/config">>).

%%% Specs

-spec start() -> {ok, pid()}.
-spec start(ConfigPath :: string()) -> {ok, pid()}.
-spec start(StartType :: atom(), StartArgs :: [term()]) -> {ok, pid()}.
-spec stop(State :: term()) -> none.

%%% API

start() ->
    start(normal, []).

start(ConfigPath) ->
    start(normal, [ConfigPath]).

%%% Application callbacks

start(normal, []) ->
    ConfigPath = get_config_path(),
    start(normal, [ConfigPath]);
start(normal, [ConfigPath]) ->
    %lager:set_loglevel(lager_console_backend, debug),
    Specs = tfsp_config:read(ConfigPath),
    tfsp_sup:start_link(Specs).

stop(_State) ->
    ok.

%%% Utilities

%% Returns path to tfsp configuration file.
%%
%% TODO: user-defined config dir
get_config_path() ->
    {ok, [[Home | _]]} = init:get_argument(home),
    filename:join(Home, ?DEFAULT_CONFIG_PATH).
