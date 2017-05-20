%%% Event queue for a tfsp event manager, used as a testing utility.
-module(event_queue).
-behaviour(gen_event).

-export([start_link/0,
         stop/1,
         clear/1,
         verify_strict/2,
         verify_loose/2]).

-export([init/1,
         terminate/2,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-include("event.hrl").


%% Specs

-record(event_queue_st, { queue :: queue:queue() }). % TODO: queue(event())

-spec start_link() -> {ok, pid()}.
-spec stop(pid()) -> ok.

-type event_spec() :: atom() | event().

-spec verify_strict(pid(), [event_spec()]) -> ok
                                            | {spec_mismatch, event_spec(), event()}
                                            | {spec_unfinished, event_spec()}.
-spec verify_loose(pid(), [event_spec()]) -> ok | {spec_unfinished, event_spec()}.
-spec clear(pid()) -> ok.


%% API

start_link() ->
    {ok, Pid} = tfsp_event:start_link(),
    ok = tfsp_event:add_handler(Pid, ?MODULE, []),
    {ok, Pid}.

stop(Pid) ->
    ok = tfsp_event:stop(Pid).

clear(Pid) ->
    gen_event:call(Pid, ?MODULE, clear).

% Verifies a list of event specifications in strict order,
% with unspecified events NOT allowed between specified events.
verify_strict(_Pid, []) ->
    ok;
verify_strict(Pid, [EventSpec | EventSpecs]) ->
    case pull(Pid) of
        {ok, {Type, Msg} = Event} ->
            case EventSpec of
                {TypeSpec, MsgSpec} ->
                    if Type =:= TypeSpec andalso Msg =:= MsgSpec ->
                           verify_strict(Pid, EventSpecs);
                       true ->
                           {spec_mismatch, EventSpec, Event}
                    end;
                TypeSpec ->
                    if Type =:= TypeSpec ->
                           verify_strict(Pid, EventSpecs);
                       true ->
                           {spec_mismatch, EventSpec, Event}
                    end
            end;
        none ->
            {spec_unfinished, EventSpec}
    end.

% Verifies a list of event specifications in loose order,
% with unspecified events allowed between specified events.
verify_loose(_Pid, []) ->
    ok;
verify_loose(Pid, [EventSpec | EventSpecs]) ->
    case pull(Pid) of
        {ok, {Type, Msg}} ->
            case EventSpec of
                {TypeSpec, MsgSpec} ->
                    if Type =:= TypeSpec andalso Msg =:= MsgSpec ->
                           verify_loose(Pid, EventSpecs);
                       true ->
                           verify_loose(Pid, [EventSpec | EventSpecs])
                    end;
                TypeSpec ->
                    if Type =:= TypeSpec ->
                           verify_loose(Pid, EventSpecs);
                       true ->
                           verify_loose(Pid, [EventSpec | EventSpecs])
                    end
            end;
        none ->
            {spec_unfinished, EventSpec}
    end.


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
    end;
handle_call(clear, _St) ->
    St = #event_queue_st{ queue = queue:new() },
    {ok, ok, St}.

handle_info(_Msg, St) ->
    {ok, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


%% Utilities

pull(Pid) ->
    gen_event:call(Pid, ?MODULE, pull).
