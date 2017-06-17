%%%-------------------------------------------------------------------
%%% tfsp SSH client module
%%%
%%% Implements SSH channel callbacks for handling data buffering
%%% and message passing on one end of a tfsp connection.
%%%-------------------------------------------------------------------

-module(tfsp_ssh_client).

-behaviour(ssh_channel).

%% API
-export([start_link/5]).

%% SSH channel callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_msg/2,
         handle_ssh_msg/2,
         terminate/2,
         code_change/3]).

%%% Constants

%% TODO: define these elsewhere?
-define(CONNECT_TIMEOUT,            5000).
-define(SESSION_CHANNEL_TIMEOUT,    5000).
-define(SUBSYSTEM_TIMEOUT,          5000).

%%% Records

-record(tfsp_ssh_client_state, { root           :: string(),
                                 sync_options   :: [term()] }).

%%% Specs

-type tfsp_ssh_client_state() :: #tfsp_ssh_client_state{}.

-spec start_link(Root           :: string(),
                 Host           :: string(),
                 Port           :: non_neg_integer(),
                 SshOptions     :: [term()],
                 SyncOptions    :: [term()]) ->
    {ok, ChannelRef :: pid()} | {error, Reason :: term()}.
-spec init([term()]) -> {ok, State :: tfsp_ssh_client_state()}.

%% API

start_link(Root, Host, Port, SshOptions, SyncOptions) ->
    case ssh:connect(Host, Port, SshOptions, ?CONNECT_TIMEOUT) of
        {ok, ConnRef} ->
            case ssh_connection:session_channel(ConnRef, ?SESSION_CHANNEL_TIMEOUT) of
                {ok, ChanId} ->
                    ssh_channel:start_link(ConnRef, ChanId, ?MODULE, [Root, SyncOptions]);
                {error, Reason} ->
                    ok = ssh:close(ConnRef),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% SSH channel callbacks

init([Root, SyncOptions]) ->
    State = #tfsp_ssh_client_state{ root            = Root,
                                    sync_options    = SyncOptions },
    {ok, State}.

handle_call(Request, From, State) ->
    lager:warning("Unhandled call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Request, State) ->
    lager:warning("Unhandled cast: ~p", [Request]),
    {noreply, State}.

handle_msg({ssh_channel_up, ChanId, ConnRef}, #tfsp_ssh_client_state{ root = Root } = State) ->
    lager:info("~s: SSH client channel up", [Root]),
    case ssh_connection:subsystem(ConnRef, ChanId, tfsp_ssh_server:subsystem_name(), ?SUBSYSTEM_TIMEOUT) of
        success ->
            {ok, State};
        failure ->
            {stop, tfsp_ssh_client_subsystem_failed};
        {error, Reason} ->
            {stop, {tfsp_ssh_client_subsystem_failed, Reason}}
    end;
handle_msg(Message, State) ->
    lager:warning("Unhandled message: ~p", [Message]),
    {ok, State}.

handle_ssh_msg({ssh_cm, _ConnRef, {data, _ChanId, 0, Data}}, State) ->
    lager:debug("Received ~p bytes from server", [erlang:byte_size(Data)]),
    {ok, State};
handle_ssh_msg({ssh_cm, _ConnRef, {eof, _ChanId}}, State) ->
    lager:debug("Received EOF from server", []),
    {ok, State};
handle_ssh_msg(Message, State) ->
    lager:warning("Unhandled SSH message: ~p", [Message]),
    {ok, State}.

terminate(Reason, _State) ->
    lager:warning("Unhandled termination: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
