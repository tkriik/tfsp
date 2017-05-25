%%% tfsp protocol handler
%%%   - parses inbound buffered data messages from connection handler
%%%   - dispatches io workers depending on the message
%%%   - routes messages from io workers back to connection handler
-module(tfsp_proto).
-behaviour(gen_statem).

-export([start_link/2,
         stop/1]).

-export([init/1,
         terminate/3,
         callback_mode/0,
         code_change/4]).

-include("fs.hrl").


%% Specs

-record(proto_data, { conn_hdlr_ref :: pid(),
                      fs_ctx        :: fs_ctx() }).


%% API

start_link(ConnHdlrRef, FsCtx) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [ConnHdlrRef, FsCtx], []).

stop(ProtoRef) ->
    gen_statem:stop(ProtoRef).


%% gen_statem callbacks

init([ConnHdlrRef, #fs_ctx{ ev_mgr = EvMgr } = FsCtx]) ->
    Data = #proto_data{ conn_hdlr_ref   = ConnHdlrRef,
                    fs_ctx          = FsCtx },
    ok = tfsp_event:notify_proto_hdlr_up(EvMgr, [ConnHdlrRef]),
    {ok, default, Data}.

terminate(_Reason, _St, #proto_data{ fs_ctx = #fs_ctx{ ev_mgr = EvMgr }}) ->
    ok = tfsp_event:notify_proto_hdlr_down(EvMgr, []),
    ok.

callback_mode() ->
    handle_event_function.

code_change(_OldVsn, St, Data, _Extra) ->
    {ok, St, Data}.
