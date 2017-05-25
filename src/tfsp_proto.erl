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

-export([handshake/3]).

-include("debug.hrl").
-include("fs.hrl").


%% Specs

-record(proto_data, { conn_hdlr_ref :: pid(),
                      ev_mgr_ref    :: pid(),
                      ent_tab       :: fs_ent_tab(),
                      cur_path      :: fs_path() }).


%% API

start_link(ConnHdlrRef, FsCtx) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [ConnHdlrRef, FsCtx], []).

stop(ProtoRef) ->
    gen_statem:stop(ProtoRef).


%% gen_statem callbacks

init([ConnHdlrRef, #fs_ctx{ ev_mgr_ref  = EvMgrRef,
                            ent_tab     = EntTab }]) ->
    Data = #proto_data{ conn_hdlr_ref   = ConnHdlrRef,
                        ev_mgr_ref      = EvMgrRef,
                        ent_tab         = EntTab,
                        cur_path        = undefined },
    ok = tfsp_event:add_fs_tick_sup_handler(EvMgrRef, self()),
    ok = tfsp_event:notify_proto_hdlr_up(EvMgrRef, []),
    ok = start_handshake(self()),
    {ok, handshake, Data}.

terminate(_Reason, _St, #proto_data{ ev_mgr_ref = EvMgrRef }) ->
    ok = tfsp_event:notify_proto_hdlr_down(EvMgrRef, []),
    ok.

callback_mode() ->
    state_functions.

code_change(_OldVsn, St, Data, _Extra) ->
    {ok, St, Data}.


%% State callbacks

handshake(cast, start_handshake, #proto_data{ ev_mgr_ref    = EvMgrRef,
                                              ent_tab       = EntTab } = Data) ->
    tfsp_event:notify_proto_hdlr_handshake(EvMgrRef, []),
    case fs_ent_tab:first(EntTab) of
        {ok, Path} ->
            ok = cont_handshake(self()),
            {keep_state, Data#proto_data{ cur_path = Path }};
        none ->
            {next_state, sync_loop, Data#proto_data{ cur_path = undefined }}
    end;
handshake(cast, cont_handshake, #proto_data{ ent_tab    = EntTab,
                                             cur_path   = CurPath } = Data) ->
    %% TODO: send current path entity
    case fs_ent_tab:next(EntTab, CurPath) of
        {ok, Path} ->
            ok = cont_handshake(self()),
            {keep_state, Data#proto_data{ cur_path = Path }};
        none ->
            {next_state, sync_loop, Data#proto_data{ cur_path = undefined }}
    end;
handshake(EventType, EventContent, Data) ->
    ?DEBUG([{event_type, EventType},
            {event_content, EventContent},
            {data, Data}]),
    {keep_state, Data}.


%% Utilities

start_handshake(ProtoRef) ->
    gen_statem:cast(ProtoRef, start_handshake).

cont_handshake(ProtoRef) ->
    gen_statem:cast(ProtoRef, cont_handshake).
