%%%-------------------------------------------------------------------
%%% tfsp configuration module.
%%%
%%% Reads and parses a tfsp configuration file.
%%%-------------------------------------------------------------------

-module(tfsp_config).

%% API
-export([read/1]).


%%% Specs

-spec read(Path :: string()) -> Specs :: [term()].


%%% API

%% Reads the configuration file at Path and returns a list of
%% application-defined synchronization specifications.
%%
%% TODO: file permission check, entry validation, spec documentation
read(Path) ->
    {ok, Specs} = file:consult(Path),
    Specs.
