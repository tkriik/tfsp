%%% Event queue for a tfsp event manager, used as a testing utility.
-module(event_queue).
-behaviour(gen_event).

-export([start_link/0,
         stop/1,

         assert_immediate/2,
         assert_immediate_/2,
         assert_immediate_type/2,
         assert_immediate_type_/2,
         assert_immediate_end/1,
         assert_immediate_end_/1]).

-export([init/1,
         terminate/2,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").


%% Specs

-record(event_queue_st, { queue :: queue:queue() }). % TODO: queue(event())

-spec start_link() -> {ok, pid()}.
-spec stop(pid()) -> ok.


%% API

start_link() ->
    {ok, Pid} = tfsp_event:start_link(),
    ok = tfsp_event:add_handler(Pid, ?MODULE, []),
    {ok, Pid}.

stop(Pid) ->
    ok = tfsp_event:stop(Pid).


%% Direct assertions

assert_immediate(Event, Pid) ->
    ?assertEqual({ok, Event}, pull(Pid)).

assert_immediate_type(EventType, Pid) ->
    ?assertMatch({ok, {EventType, _}}, pull(Pid)).

assert_immediate_end(Pid) ->
    ?assertEqual(none, pull(Pid)).


%% Test generators

assert_immediate_(Event, Pid) ->
    ?_assertEqual({ok, Event}, pull(Pid)).

assert_immediate_type_(EventType, Pid) ->
    ?_assertMatch({ok, {EventType, _}}, pull(Pid)).

assert_immediate_end_(Pid) ->
    ?_assertEqual(none, pull(Pid)).


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


%% Utilities

pull(Pid) ->
    gen_event:call(Pid, ?MODULE, pull).
