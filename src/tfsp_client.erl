%%%-------------------------------------------------------------------
%%% tfsp client process module
%%%
%%% Maintains a connection to a server, whether possible or not.
%%% Does not handle any calls or casts.
%%%-------------------------------------------------------------------

-module(tfsp_client).

-behaviour(gen_server).

%% API
-export([start_link/4,
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
-define(INIT_RETRY_INTERVAL, 15).
-define(MAX_RETRY_INTERVAL, 300).

%%% Records

-record(tfsp_client_state, { root               :: tfsp_path(),
                             ent_tab_ref        :: tfsp_ent_tab_ref(),
                             connect_transport  :: tuple(),
                             sync_options       :: [term()],
                             conn_handler_ref   :: pid() | undefined,
                             retry_interval     :: non_neg_integer() }).

%%% Specs

-type tfsp_path() :: tfsp_file:path().
-type tfsp_ent_tab_ref() :: tfsp_ent_tab:tfsp_ent_tab_ref().
-type tfsp_client_state() :: #tfsp_client_state{}.

-spec start_link(Root               :: tfsp_path(),
                 EntTabRef          :: tfsp_ent_tab_ref(),
                 ConnectTransport   :: tuple(),
                 SyncOptions        :: [term()]) ->
    {ok, ClientRef :: pid()}.
-spec stop(ClientRef :: pid()) -> ok.

-spec init([term()]) -> {ok, State :: tfsp_client_state()}.

%%% API

%% Starts and links to tfsp client process with given root path,
%% connect transport and sync options.
start_link(Root, EntTabRef, ConnectTransport, SyncOptions) ->
    gen_server:start_link(?SERVER_NAME, [Root, EntTabRef, ConnectTransport, SyncOptions], []).

%% Casts a connect command message to a client, used by itself.
start_connect(ClientRef) ->
    gen_server:cast(ClientRef, connect).

%% Stops a tfsp client process.
stop(ClientRef) ->
    exit(ClientRef, normal),
    ok.

%%% gen_server callbacks

init([Root, EntTabRef, ConnectTransport, SyncOptions]) ->
    process_flag(trap_exit, true),
    start_connect(self()),
    State = #tfsp_client_state{ root                = Root,
                                ent_tab_ref         = EntTabRef,
                                connect_transport   = ConnectTransport,
                                sync_options        = SyncOptions,
                                conn_handler_ref    = undefined,
                                retry_interval      = ?INIT_RETRY_INTERVAL },
    {ok, State}.

handle_call(Request, From, State) ->
    lager:warning("Unhandled call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(connect, #tfsp_client_state{ root               = Root,
                                         sync_options       = SyncOptions,
                                         connect_transport  = ConnectTransport,
                                         conn_handler_ref   = undefined,
                                         retry_interval     = RetryInterval } = State) ->
    NewState = case ConnectTransport of
                   {ssh, Host, Port, SshOptions} ->
                       case tfsp_ssh_client:start_link(Root, Host, Port, SshOptions, SyncOptions) of
                           {ok, ChannelRef} ->
                               State#tfsp_client_state{ conn_handler_ref = ChannelRef };
                           {error, Reason} ->
                               lager:warning("~s: Failed to start SSH client with "
                                             "host ~p and port ~p: ~p. "
                                             "Retrying in ~p seconds...",
                                             [Root, Host, Port, Reason, RetryInterval]),
                               defer_start_connect(self(), RetryInterval),
                               NextRetryInterval = case RetryInterval  * 2 < ?MAX_RETRY_INTERVAL of
                                                       true     -> RetryInterval * 2;
                                                       false    -> ?MAX_RETRY_INTERVAL
                                                   end,
                               State#tfsp_client_state{ retry_interval = NextRetryInterval }
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
