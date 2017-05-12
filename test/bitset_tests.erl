%%% Bitset constructor and operation tests

-module(bitset_tests).

-include_lib("eunit/include/eunit.hrl").


%% Main test

module_test_() ->
    {"Bitset",
     [{"construction", fun test_construct/0},
      {"width", fun test_width/0},
      {"read", fun test_read/0},
      {"write", fun test_write/0},
      {"population count", fun test_popcount/0}
     ]
    }.


%% Test routines

test_construct() ->
    A = bitset:new(0),
    ?assertEqual({0, <<>>}, A),
    B = bitset:new(1000),
    ?assertEqual({0, <<0:1000>>}, B).

test_width() ->
    A = bitset:new(0),
    ?assertEqual(0, bitset:width(A)),
    B = bitset:new(1000),
    ?assertEqual(1000, bitset:width(B)).

test_read() ->
    A = bitset:new(1000),
    ?assertEqual(0, bitset:get_at(0, A)),
    ?assertEqual(0, bitset:get_at(999, A)).

test_write() ->
    A = bitset:new(1000),
    ?assertEqual(0, bitset:get_at(0, A)),
    ?assertEqual(0, bitset:get_at(1, A)),
    ?assertEqual(0, bitset:get_at(999, A)),
    B = bitset:set_at(0, A),
    ?assertEqual(1, bitset:get_at(0, B)),
    ?assertEqual(0, bitset:get_at(1, B)),
    ?assertEqual(0, bitset:get_at(999, B)),
    C = bitset:set_at(1, B),
    ?assertEqual(1, bitset:get_at(0, C)),
    ?assertEqual(1, bitset:get_at(1, C)),
    ?assertEqual(0, bitset:get_at(999, C)),
    D = bitset:set_at(999, C),
    ?assertEqual(1, bitset:get_at(0, D)),
    ?assertEqual(1, bitset:get_at(1, D)),
    ?assertEqual(1, bitset:get_at(999, D)).

test_popcount() ->
    A = bitset:new(1000),
    ?assertEqual(0, bitset:popcount(A)),
    B = bitset:set_at(0, A),
    ?assertEqual(1, bitset:popcount(B)),
    C = bitset:set_at(999, B),
    ?assertEqual(2, bitset:popcount(C)).
