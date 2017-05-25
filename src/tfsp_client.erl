-module(tfsp_client).

-export([start_link_ssh/5,
         stop/1]).

-include("fs.hrl").


%% Specs

-spec start_link_ssh(fs_ctx(),
                     string(),
                     non_neg_integer(),
                     non_neg_integer(),
                     [term()]) -> {ok, pid()} | {error, term()}.
-spec stop(pid()) -> ok.


%% API

start_link_ssh(FsCtx, Host, Port, Timeout, SshOpts) ->
    tfsp_ssh_client:start_link(FsCtx, Host, Port, Timeout, SshOpts).

stop(ClientRef) ->
    true = exit(ClientRef, normal),
    ok.
