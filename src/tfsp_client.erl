%%%-------------------------------------------------------------------
%%% tfsp client process module
%%%
%%% Maintains a connection to a server, whether possible or not.
%%% Does not handle any calls or casts.
%%%-------------------------------------------------------------------

-module(tfsp_client).

-behaviour(gen_server).

%% API
-export([start_link/3,
         start_connect/1,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%% Constants

-define(SERVER_NAME, ?MODULE).
-define(RETRY_INTERVAL, 5).

%%% Records

-record(tfsp_client_state, { root               :: string(),
                             connect_transport  :: tuple(),
                             sync_options       :: [term()],
                             conn_handler_ref   :: pid() | undefined }).

%%% Specs

-type tfsp_client_state() :: #tfsp_client_state{}.

-spec start_link(Root               :: string(),
                 ConnectTransport   :: tuple(),
                 SyncOptions        :: [term()]) ->
    {ok, ClientRef :: pid()}.
-spec stop(ClientRef :: pid()) -> ok.

-spec init([term()]) -> {ok, State :: tfsp_client_state()}.

%%% API

%% Starts and links to tfsp client process with given root path,
%% connect transport and sync options.
start_link(Root, ConnectTransport, SyncOptions) ->
    gen_server:start_link(?SERVER_NAME, [Root, ConnectTransport, SyncOptions], []).

%% Casts a connect command message to a client, used by itself.
start_connect(ClientRef) ->
    gen_server:cast(ClientRef, connect).

%% Stops a tfsp client process.
stop(ClientRef) ->
    exit(ClientRef, normal),
    ok.

%%% gen_server callbacks

init([Root, ConnectTransport, SyncOptions]) ->
    process_flag(trap_exit, true),
    start_connect(self()),
    State = #tfsp_client_state{ root                = Root,
                                connect_transport   = ConnectTransport,
                                sync_options        = SyncOptions,
                                conn_handler_ref    = undefined },
    {ok, State}.

handle_call(Request, From, State) ->
    lager:warning("Unhandled call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(connect, #tfsp_client_state{ root               = Root,
                                         sync_options       = SyncOptions,
                                         connect_transport  = ConnectTransport,
                                         conn_handler_ref   = undefined } = State) ->
    NewState = case ConnectTransport of
                   {ssh, Host, Port, SshOptions} ->
                       case tfsp_ssh_client:start_link(Root, Host, Port, SshOptions, SyncOptions) of
                           {ok, ChannelRef} ->
                               State#tfsp_client_state{ conn_handler_ref = ChannelRef };
                           {error, Reason} ->
                               lager:warning("~s: Failed to start SSH client with "
                                             "host ~p and port ~p: ~p. "
                                             "Retrying in ~p seconds...",
                                             [Host, Port, Reason, ?RETRY_INTERVAL]),
                               defer_start_connect(self(), ?RETRY_INTERVAL),
                               State
                       end
               end,
    {noreply, NewState};
handle_cast(Request, State) ->
    lager:warning("Unhandled cast: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    % TODO: close connection
    lager:warning("Unhandled termination: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Utilities

defer_start_connect(ClientRef, Time) ->
    {ok, _TRef} = timer:apply_after(Time * 1000, ?MODULE, start_connect, [ClientRef]),
    ok.
