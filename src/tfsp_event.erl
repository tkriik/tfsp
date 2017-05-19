%%% tfsp event manager, mainly used for testing, debugging and logging.
-module(tfsp_event).

-export([start_link/0,
         add_handler/3]).


%% Specs

-spec start_link() -> {ok, pid()}.
-spec add_handler(pid(), atom(), term()) -> ok | {error, term()}.


%% API

start_link() ->
    gen_event:start_link().

add_handler(Pid, Module, Args) ->
    gen_event:add_handler(Pid, Module, Args).
