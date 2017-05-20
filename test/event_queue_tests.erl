%%% Tests for the event queue testing module.
-module(event_queue_tests).

-include_lib("eunit/include/eunit.hrl").


%% Main test

module_test_() ->
    {"event queue assertion interface",
     {foreach, local, fun setup/0, fun cleanup/1,
      [fun assert_immediate_empty/1,
       fun assert_immediate_one/1,
       fun assert_immediate_two/1,
       fun assert_immediate_type_one/1,
       fun assert_immediate_type_two/1]}
    }.


%% Fixtures

setup() ->
    {ok, Pid} = event_queue:start_link(),
    Pid.

cleanup(Pid) ->
    event_queue:stop(Pid).


%% Tests

assert_immediate_empty(Pid) ->
    {"immediate end with an empty event queue",
     event_queue:assert_immediate_end_(Pid)}.

assert_immediate_one(Pid) ->
    ok = tfsp_event:notify_misc(Pid, testing_assert_immediate_one),
    {"immediate equality with one event",
     [event_queue:assert_immediate_({misc, testing_assert_immediate_one}, Pid),
      event_queue:assert_immediate_end_(Pid)]}.

assert_immediate_two(Pid) ->
    ok = tfsp_event:notify_misc(Pid, testing_assert_immediate_two_first),
    ok = tfsp_event:notify_misc(Pid, testing_assert_immediate_two_second),
    {"immediate equality with two events",
     [event_queue:assert_immediate_({misc, testing_assert_immediate_two_first}, Pid),
      event_queue:assert_immediate_({misc, testing_assert_immediate_two_second}, Pid),
      event_queue:assert_immediate_end_(Pid)]}.

assert_immediate_type_one(Pid) ->
    ok = tfsp_event:notify_misc(Pid, testing_assert_immediate_type_one),
    {"immediate event type match with one event",
     [event_queue:assert_immediate_type_(misc, Pid),
      event_queue:assert_immediate_end_(Pid)]}.

assert_immediate_type_two(Pid) ->
    ok = tfsp_event:notify_misc(Pid, testing_assert_immediate_type_two_first),
    ok = tfsp_event:notify_misc(Pid, testing_assert_immediate_type_two_second),
    {"immediate event type match with two events",
     [event_queue:assert_immediate_type_(misc, Pid),
      event_queue:assert_immediate_type_(misc, Pid),
      event_queue:assert_immediate_end_(Pid)]}.
