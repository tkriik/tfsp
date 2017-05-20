%%% tfsp event manager, mainly used for testing, debugging and logging.
-module(tfsp_event).

-export([start_link/0,
         stop/1,
         add_handler/3,
         notify_misc/2]).


%% Specs

-spec start_link() -> {ok, pid()}.
-spec add_handler(pid(), atom(), term()) -> ok | {error, term()}.

-spec notify_misc(pid(), term()) -> ok.


%% API

start_link() ->
    gen_event:start_link().

stop(Pid) ->
    gen_event:stop(Pid).

add_handler(Pid, Module, Args) ->
    gen_event:add_handler(Pid, Module, Args).

notify_misc(Pid, Event) ->
    notify(Pid, {misc, Event}).


%% Utilities

notify(Pid, Event) ->
    gen_event:notify(Pid, Event).
