%%% tfsp event manager, mainly used for testing, debugging and logging.
-module(tfsp_event).

-export([start_link/0,
         stop/1,

         add_handler/3,
         add_fs_tick_sup_handler/2,

         notify_fs_ent_created/2,
         notify_fs_ent_deleted/2,
         notify_proto_hdlr_up/2,
         notify_proto_hdlr_down/2,
         notify_ssh_client_chan_up/2,
         notify_misc/2]).

-include("fs.hrl").


%% Specs

-spec start_link() -> {ok, pid()}.
-spec add_handler(pid(), atom(), term()) -> ok | {error, term()}.
-spec add_fs_tick_sup_handler(pid(), pid()) -> ok | {error, term()}.
-spec stop(pid()) -> ok.

-spec notify(pid(), term()) -> ok.


%% API

start_link() ->
    gen_event:start_link().

stop(EvMgrRef) ->
    gen_event:stop(EvMgrRef).

add_handler(EvMgrRef, Module, Args) ->
    gen_event:add_handler(EvMgrRef, Module, Args).

add_fs_tick_sup_handler(EvMgrRef, RecvRef) ->
    gen_event:add_sup_handler(EvMgrRef, fs_tick, [RecvRef]).

notify_fs_ent_created(EvMgrRef, Ent) ->
    notify(EvMgrRef, {fs_ent_created, Ent}).

notify_fs_ent_deleted(EvMgrRef, Ent) ->
    notify(EvMgrRef, {fs_ent_deleted, Ent}).

notify_proto_hdlr_up(EvMgrRef, Extra) ->
    notify(EvMgrRef, {proto_hdlr_up, Extra}).

notify_proto_hdlr_down(EvMgrRef, Extra) ->
    notify(EvMgrRef, {proto_hdlr_down, Extra}).

notify_ssh_client_chan_up(EvMgrRef, Extra) ->
    notify(EvMgrRef, {ssh_client_chan_up, Extra}).

notify_misc(EvMgrRef, Event) ->
    notify(EvMgrRef, {misc, Event}).


%% Utilities

notify(EvMgrRef, Event) ->
    gen_event:notify(EvMgrRef, Event).
