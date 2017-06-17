%%%-------------------------------------------------------------------
%%% tfsp SSH server module
%%%
%%% Implements SSH channel daemon callbacks for handling data buffering
%%% and message passing on one end of a tfsp connection.
%%%-------------------------------------------------------------------

-module(tfsp_ssh_server).

-behaviour(ssh_daemon_channel).

%% API
-export([start_daemon/4,
         stop_daemon/1,
         subsystem_name/0]).

%% SSH daemon channel callbacks
-export([init/1,
         handle_msg/2,
         handle_ssh_msg/2,
         terminate/2]).

%%% Records

-record(tfsp_ssh_server_state, { root           :: string(),
                                 sync_options   :: [term()] }).

%%% Specs

-type tfsp_ssh_server_state() :: #tfsp_ssh_server_state{}.

-spec start_daemon(Root         :: string(),
                   Port         :: non_neg_integer(),
                   SshOptions   :: [term()],
                   SyncOptions  :: [term()]) ->
    {ok, DaemonRef :: ssh:ssh_daemon_ref()} | {error, Reason :: term()}.

-spec stop_daemon(DaemonRef :: ssh:ssh_daemon_ref()) -> ok | {error, Reason :: term()}.

-spec init(Args :: [term()]) -> {ok, tfsp_ssh_server_state()}.

%%% API

start_daemon(Root, Port, SshOptions, SyncOptions) ->
    Subsystem = {subsystem_name(), {?MODULE, [Root, SyncOptions]}},
    AdditionalSshOptions = [{subsystems, [Subsystem]},
                            {ssh_cli, no_cli}],
    NewSshOptions = lists:append(AdditionalSshOptions, SshOptions),
    ssh:daemon(Port, NewSshOptions).

stop_daemon(DaemonRef) ->
    ssh:stop_daemon(DaemonRef).

subsystem_name() ->
    "tfsp".

%%% SSH daemon channel callbacks

init([Root, SyncOptions]) ->
    State = #tfsp_ssh_server_state{ root            = Root,
                                    sync_options    = SyncOptions },
    {ok, State}.

handle_msg({ssh_channel_up, _ChanId, _ConnRef}, #tfsp_ssh_server_state{ root = Root } = State) ->
    lager:info("~s: SSH server channel up", [Root]),
    {ok, State};
handle_msg(Message, State) ->
    lager:warning("Unhandled message: ~p", [Message]),
    {ok, State}.

handle_ssh_msg({ssh_cm, _ConnRef, {data, _ChanId, 0, Data}}, State) ->
    lager:debug("Received ~p bytes from server", [erlang:byte_size(Data)]),
    {ok, State};
handle_ssh_msg(Message, State) ->
    lager:warning("Unhandled SSH message: ~p", [Message]),
    {ok, State}.

terminate(Reason, _State) ->
    lager:warning("Unhandled termination: ~p", [Reason]),
    ok.
