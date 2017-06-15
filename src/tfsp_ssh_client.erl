-module(tfsp_ssh_client).
-behaviour(ssh_channel).

-export([start_link/5]).

-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_msg/2,
         handle_ssh_msg/2,
         code_change/3]).

-include("conn.hrl").
-include("fs.hrl").


%%% Specs

-spec start_link(fs_ctx(),
                 string(),
                 integer(),
                 integer() | infinity,
                 [term()]) -> {ok, pid()} | {error, term()}.


%%% API

start_link(FsCtx, Host, Port, Timeout, SshOpts) ->
    case ssh:connect(Host, Port, SshOpts, Timeout) of
        {ok, ConnRef} ->
            case ssh_connection:session_channel(ConnRef, Timeout) of
                {ok, ChanId} ->
                    ssh_channel:start_link(ConnRef, ChanId, ?MODULE, [FsCtx, Timeout]);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%%% SSH channel callbacks

init([FsCtx, Timeout]) ->
    St = #conn_st{ fs_ctx   = FsCtx,
                   timeout  = Timeout,
                   buffer   = <<>> },
    {ok, St}.

terminate(_Reason, _St) ->
    ok.

handle_call(_Msg, _From, St) ->
    {noreply, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_msg({ssh_channel_up, ChanId, ConnRef},
           #conn_st{ fs_ctx     = #fs_ctx{ ev_mgr_ref = EvMgrRef },
                     timeout    = Timeout } = St) ->
    case ssh_connection:subsystem(ConnRef,
                                  ChanId,
                                  tfsp_ssh_server:subsystem_name(),
                                  Timeout) of
        success ->
            tfsp_event:notify_ssh_client_chan_up(EvMgrRef, [ChanId, ConnRef]),
            {ok, St};
        failure ->
            {stop, ChanId, St};
        {error, _Reason} ->
            {stop, ChanId, St}
    end;
handle_msg(_Msg, St) ->
    {ok, St}.

%% Got data from channel, append to connection state buffer.
handle_ssh_msg({ssh_cm, _ConnRef, {data, _ChanId, 0, Data}},
               #conn_st { buffer = Buffer } = St) ->
    _Buffer = <<Buffer/binary, Data/binary>>,
    _St = St#conn_st{ buffer = _Buffer },
    {ok, _St};
%% Got EOF from channel, flush connection state buffer.
handle_ssh_msg({ssh_cm, _ConnRef, {eof, _ChanId}}, St) ->
    _St = St#conn_st{ buffer = <<>> },
    {ok, _St};
handle_ssh_msg(_Msg, St) ->
    {ok, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
