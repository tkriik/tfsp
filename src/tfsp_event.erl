%%% tfsp event manager, mainly used for testing, debugging and logging.
-module(tfsp_event).

-export([start_link/0,
         stop/1,
         add_handler/3,

         notify_fs_ent_created/2,
         notify_fs_ent_deleted/2,
         notify_ssh_client_chan_up/2,
         notify_misc/2]).

-include("fs.hrl").


%% Specs

-spec start_link() -> {ok, pid()}.
-spec add_handler(pid(), atom(), term()) -> ok | {error, term()}.
-spec stop(pid()) -> ok.

-spec notify_fs_ent_created(pid(), fs_ent()) -> ok.
-spec notify_misc(pid(), term()) -> ok.


%% API

start_link() ->
    gen_event:start_link().

stop(Pid) ->
    gen_event:stop(Pid).

add_handler(Pid, Module, Args) ->
    gen_event:add_handler(Pid, Module, Args).

notify_fs_ent_created(Pid, Ent) ->
    notify(Pid, {fs_ent_created, Ent}).

notify_fs_ent_deleted(Pid, Ent) ->
    notify(Pid, {fs_ent_deleted, Ent}).

notify_ssh_client_chan_up(Pid, Extra) ->
    notify(Pid, {ssh_client_chan_up, Extra}).

notify_misc(Pid, Event) ->
    notify(Pid, {misc, Event}).


%% Utilities

notify(Pid, Event) ->
    gen_event:notify(Pid, Event).
