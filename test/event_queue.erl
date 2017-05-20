%%% Event queue for a tfsp event manager, used as a testing utility.
-module(event_queue).
-behaviour(gen_event).

-export([start_link/0,
         stop/1,
         pull/1]).

-export([init/1,
         terminate/2,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3]).


%% Specs

-record(event_queue_st, { queue :: queue:queue() }). % TODO: queue(event())


%% API

start_link() ->
    {ok, Pid} = tfsp_event:start_link(),
    ok = tfsp_event:add_handler(Pid, ?MODULE, []),
    {ok, Pid}.

stop(Pid) ->
    ok = tfsp_event:stop(Pid).

pull(Pid) ->
    gen_event:call(Pid, ?MODULE, pull).


%% gen_event callbacks

init([]) ->
    St = #event_queue_st{ queue = queue:new() },
    {ok, St}.

terminate(_Arg, _St) ->
    ok.

handle_event(Event, #event_queue_st{ queue = Q } = St) ->
    _Q = queue:in(Event, Q),
    _St = St#event_queue_st{ queue = _Q },
    {ok, _St}.

handle_call(pull, #event_queue_st{ queue = Q } = St) ->
    case queue:out(Q) of
        {{value, Event}, _Q} ->
            _St = St#event_queue_st{ queue = _Q },
            {ok, {ok, Event}, _St};
        {empty, _Q} ->
            {ok, none, St}
    end.

handle_info(_Msg, St) ->
    {ok, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
