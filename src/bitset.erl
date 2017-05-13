%%% Simple bit set module, used for marking transferred file blocks. 
 
-module(bitset). 
-export([new/1,
         width/1,
         popcount/1, 
         get_at/2,
         set_at/2]). 
 

%% Specs

-export_type([bitset/0]).

-type bitset() :: {Popcount :: non_neg_integer(), bitstring()}.


%% API

new(Size) ->
    {0, <<0:Size>>}. 

width({_, Bitstring}) ->
    erlang:bit_size(Bitstring).

popcount({Popcount, _}) ->
    Popcount. 

get_at(I, {_, Bitstring}) -> 
    fun(<<_:I/bits, Bit:1, _/bits>>) ->
            Bit 
    end(Bitstring).

set_at(I, {Popcount, Bitstring}) ->
    fun(<<L:I/bits, Bit:1, R/bits>>) ->
            {Popcount + (Bit bxor 1), <<L/bits, 1:1, R/bits>>} 
    end(Bitstring).
