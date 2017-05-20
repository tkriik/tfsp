%%% Tests for the event queue testing module.
-module(event_queue_tests).

-include_lib("eunit/include/eunit.hrl").


%% Main test

strict_test_() ->
    {"strict event queue verification",
     {foreach, local, fun setup/0, fun cleanup/1,
      [fun verify_strict_empty_with_no_specs/1,
       fun verify_strict_empty_unfinished/1,
       fun verify_strict_atom_one_ok/1,
       fun verify_strict_atom_one_mismatch/1,
       fun verify_strict_event_one_ok/1,
       fun verify_strict_mixed_many_ok/1,
       fun verify_strict_mixed_many_mismatch/1,
       fun verify_strict_mixed_many_unfinished/1
      ]}
    }.

loose_test_() ->
    {"loose event queue verification",
     {foreach, local, fun setup/0, fun cleanup/1,
      [fun verify_loose_empty_with_no_specs/1,
       fun verify_loose_empty_unfinished/1,
       fun verify_loose_atom_one_ok/1,
       fun verify_loose_event_one_ok/1,
       fun verify_loose_mixed_many_ok/1,
       fun verify_loose_mixed_many_unfinished/1
      ]}
    }.


%% Fixtures

setup() ->
    {ok, Pid} = event_queue:start_link(),
    Pid.

cleanup(Pid) ->
    event_queue:stop(Pid).


%% Tests

verify_strict_empty_with_no_specs(Pid) ->
    {"empty event queue and spec list",
     ?_assertEqual(ok, event_queue:verify_strict(Pid, []))}.

verify_loose_empty_with_no_specs(Pid) ->
    {"empty event queue and spec list",
     ?_assertEqual(ok, event_queue:verify_loose(Pid, []))}.

verify_strict_empty_unfinished(Pid) ->
    Spec = {misc, undefined_event},
    {"empty event queue and one spec",
     ?_assertEqual({spec_unfinished, Spec}, event_queue:verify_strict(Pid, [Spec]))}.

verify_loose_empty_unfinished(Pid) ->
    Spec = {misc, undefined_event},
    {"empty event queue and one spec",
     ?_assertEqual({spec_unfinished, Spec}, event_queue:verify_loose(Pid, [Spec]))}.

verify_strict_atom_one_ok(Pid) ->
    Specs = [misc],
    ok = tfsp_event:notify_misc(Pid, some_event),
    {"event queue with one event and one atom spec",
     ?_assertEqual(ok, event_queue:verify_strict(Pid, Specs))}.

verify_loose_atom_one_ok(Pid) ->
    Specs = [misc],
    ok = tfsp_event:notify_misc(Pid, some_event),
    {"event queue with one event and one atom spec",
     ?_assertEqual(ok, event_queue:verify_loose(Pid, Specs))}.

verify_strict_atom_one_mismatch(Pid) ->
    Specs = [misc],
    ok = tfsp_event:notify_fs_ent_created(Pid, some_fs_ent),
    {"event queue with one event and mismatched atom spec",
     ?_assertEqual({spec_mismatch, misc, {fs_ent_created, some_fs_ent}},
                   event_queue:verify_strict(Pid, Specs))}.

verify_strict_event_one_ok(Pid) ->
    Spec = {misc, some_event},
    ok = tfsp_event:notify_misc(Pid, some_event),
    {"event queue with one event and one event spec",
     ?_assertEqual(ok, event_queue:verify_strict(Pid, [Spec]))}.

verify_loose_event_one_ok(Pid) ->
    Spec = {misc, some_event},
    ok = tfsp_event:notify_misc(Pid, some_event),
    {"event queue with one event and one event spec",
     ?_assertEqual(ok, event_queue:verify_loose(Pid, [Spec]))}.

verify_strict_mixed_many_ok(Pid) ->
    Specs = [misc,
             {misc, event_two},
             {misc, event_three},
             misc],
    ok = tfsp_event:notify_misc(Pid, event_one),
    ok = tfsp_event:notify_misc(Pid, event_two),
    ok = tfsp_event:notify_misc(Pid, event_three),
    ok = tfsp_event:notify_misc(Pid, event_four),
    {"event queue with many events and mixed event specs",
     ?_assertEqual(ok, event_queue:verify_strict(Pid, Specs))}.

verify_loose_mixed_many_ok(Pid) ->
    Specs = [misc,
             {misc, event_two},
             {misc, event_three},
             misc],
    ok = tfsp_event:notify_misc(Pid, event_one),
    ok = tfsp_event:notify_misc(Pid, event_two),
    ok = tfsp_event:notify_misc(Pid, event_three),
    ok = tfsp_event:notify_misc(Pid, event_four),
    {"event queue with many events and mixed event specs",
     ?_assertEqual(ok, event_queue:verify_loose(Pid, Specs))}.

verify_strict_mixed_many_mismatch(Pid) ->
    Specs = [misc,
             {misc, event_two},
             misc,
             {misc, event_four}],
    ok = tfsp_event:notify_misc(Pid, event_one),
    ok = tfsp_event:notify_fs_ent_created(Pid, some_fs_ent),
    ok = tfsp_event:notify_misc(Pid, event_three),
    ok = tfsp_event:notify_misc(Pid, event_four),
    {"event queue with many events and mismatched event spec",
     ?_assertEqual({spec_mismatch, {misc, event_two}, {fs_ent_created, some_fs_ent}},
                   event_queue:verify_strict(Pid, Specs))}.


verify_strict_mixed_many_unfinished(Pid) ->
    Specs = [{misc, event_one},
             misc,
             {misc, event_three},
             {misc, event_four}],
    ok = tfsp_event:notify_misc(Pid, event_one),
    ok = tfsp_event:notify_misc(Pid, event_two),
    ok = tfsp_event:notify_misc(Pid, event_three),
    {"event queue with not enough events and mixed event specs",
     ?_assertEqual({spec_unfinished, {misc, event_four}}, event_queue:verify_strict(Pid, Specs))}.

verify_loose_mixed_many_unfinished(Pid) ->
    Specs = [{misc, event_one},
             misc,
             {misc, event_three},
             {misc, event_four}],
    ok = tfsp_event:notify_misc(Pid, event_one),
    ok = tfsp_event:notify_misc(Pid, event_two),
    ok = tfsp_event:notify_misc(Pid, event_three),
    {"event queue with not enough events and mixed event specs",
     ?_assertEqual({spec_unfinished, {misc, event_four}}, event_queue:verify_loose(Pid, Specs))}.
