%%% gen_event handler for announcing file entity events to processes.
-module(fs_tick).
-behaviour(gen_event).

-export([init/1,
         terminate/2,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         code_change/3]).


%% Specs

-record(fs_tick_st, { recv_ref :: pid() }).


%% API


%% gen_event callbacks

init([RecvRef]) ->
    {ok, #fs_tick_st{ recv_ref = RecvRef }}.

terminate(_Reason, _St) ->
    ok.

handle_call(_Req, _St) ->
    {ok, undefined, _St}.

handle_event({Type, _Data} = Event, #fs_tick_st{ recv_ref = RecvRef } = St) ->
    case lists:member(Type, [fs_ent_created, fs_ent_deleted]) of
        true ->
            RecvRef ! Event;
        false ->
            ok
    end,
    {ok, St}.

handle_info(_Info, St) ->
    {ok, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
