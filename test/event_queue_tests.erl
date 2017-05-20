%%% Tests for the event queue testing module.
-module(event_queue_tests).

-include_lib("eunit/include/eunit.hrl").


%% Main test

module_test_() ->
    {"event queue interface",
     {foreach, local, fun setup/0, fun cleanup/1,
      [fun empty_queue/1,
       fun notify_pull_one/1,
       fun notify_pull_two/1]}
    }.


%% Fixtures

setup() ->
    {ok, Pid} = event_queue:start_link(),
    Pid.

cleanup(Pid) ->
    event_queue:stop(Pid).


%% Tests

empty_queue(Pid) ->
    {"pulling from an empty event queue",
     ?_assertEqual(none, event_queue:pull(Pid))}.

notify_pull_one(Pid) ->
    ok = tfsp_event:notify_misc(Pid, testing_notify_pull_one),
    {"pulling from a queue with one event",
     [?_assertEqual({ok, {misc, testing_notify_pull_one}}, event_queue:pull(Pid)),
      ?_assertEqual(none, event_queue:pull(Pid))]}.

notify_pull_two(Pid) ->
    ok = tfsp_event:notify_misc(Pid, testing_notify_pull_two_first),
    ok = tfsp_event:notify_misc(Pid, testing_notify_pull_two_second),
    {"pulling from a queue with two events",
     [?_assertEqual({ok, {misc, testing_notify_pull_two_first}}, event_queue:pull(Pid)),
      ?_assertEqual({ok, {misc, testing_notify_pull_two_second}}, event_queue:pull(Pid)),
      ?_assertEqual(none, event_queue:pull(Pid))]}.
