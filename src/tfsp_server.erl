%%%-------------------------------------------------------------------
%%% tfsp server process module
%%%
%%% Maintains a reference to an active listener process.
%%% Does not handle any calls or casts.
%%%-------------------------------------------------------------------

-module(tfsp_server).

-behaviour(gen_server).

%% API
-export([start_link/4,
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


%%% Records

-record(tfsp_server_state, { root               :: string(),
                             serve_transport    :: tuple(),
                             sync_options       :: [term()],
                             conn_handler_ref   :: pid() | any() }).

%%% Specs

-type tfsp_path() :: tfsp_file:path().
-type tfsp_ent_tab_ref() :: tfsp_ent_tab:tfsp_ent_tab_ref().
-type tfsp_server_state() :: #tfsp_server_state{}.

-spec start_link(Root           :: tfsp_path(), 
                 EntTabRef      :: tfsp_ent_tab_ref(),
                 ServeTransport :: tuple(),
                 SyncOptions    :: [term()]) ->
    {ok, ServerRef :: pid()}.
-spec stop(ServerRef :: pid()) -> ok.

-spec init([term()]) -> {ok, State :: tfsp_server_state()}.

%%% API

%% Starts and links to tfsp server process with given root path,
%% serve transport and sync options.
start_link(Root, EntTabRef, ServeTransport, SyncOptions) ->
    gen_server:start_link(?SERVER_NAME, [Root, ServeTransport, SyncOptions], []).

stop(ServerRef) ->
    gen_server:stop(ServerRef).

%%% gen_server callbacks

init([Root, ServeTransport, SyncOptions]) ->
    process_flag(trap_exit, true),
    State = #tfsp_server_state{ root            = Root,
                                serve_transport = ServeTransport,
                                sync_options    = SyncOptions },
    _State = case ServeTransport of
                 {ssh, Port, SshOptions} ->
                     case tfsp_ssh_server:start_daemon(Root, Port, SshOptions, SyncOptions) of
                         {ok, DaemonRef} ->
                             lager:info("~s: SSH server started on port ~p", [Root, Port]),
                             State#tfsp_server_state{ conn_handler_ref = DaemonRef };
                         {error, Reason} ->
                             lager:error("~s: Failed to start SSH server on port ~p: ~p",
                                         [Root, Port, Reason]),
                             {stop, {tfsp_ssh_server_failed, Reason}}
                     end
             end,
    {ok, _State}.

handle_call(Request, From, State) ->
    lager:warning("Unhandled call from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(Request, State) ->
    lager:warning("Unhandled cast: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    % TODO: close daemon
    lager:warning("Unhandled termination: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
