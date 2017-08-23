-module(erl_iputils).

-export([parse_range/1]).
-export([ranges_overlap/2]).
-export([ipv4_to_int/1]).
-export([int_to_ipv4/1]).


-spec parse_range(string() | binary()) -> {ok, {RangeStart, RangeEnd}} | {error, term()} when
      RangeStart :: non_neg_integer(),
      RangeEnd   :: non_neg_integer().
parse_range(Range) when is_binary(Range) ->
    parse_range(binary_to_list(Range));
parse_range(Range) when is_list(Range) ->
    {Base, RawSubnet} = case string:tokens(Range, "/") of
        [B, S] -> {B, S};
        [B]    -> {B, "32"}
    end,
    Subnet = list_to_integer(RawSubnet),
    catch begin
        Ip = case inet:parse_address(Base) of
            {ok, {_, _, _, _} = I} -> I;
            {ok, _} -> throw({error, not_ipv4});
            {error, _} = Err -> throw(Err)
        end,
        IntIp = ipv4_to_int(Ip),
        SubnetShift = 32 - Subnet,
        RangeStart = (IntIp bsr SubnetShift) bsl SubnetShift,
        WildCard = lists:foldl(fun(_, Acc) -> (Acc bsl 1) bor 1 end, 0, lists:seq(0, SubnetShift - 1)),
        RangeEnd = RangeStart bor WildCard,
        {ok, {RangeStart, RangeEnd}}
    end.


-spec ranges_overlap(Range1, Range2) -> boolean() when
      Range1 :: {Range1Start :: non_neg_integer(), Range1End :: non_neg_integer()},
      Range2 :: {Range2Start :: non_neg_integer(), Range2End :: non_neg_integer()}.
ranges_overlap({Range1Start, Range1End}, {Range2Start, Range2End}) ->
    max(Range1Start, Range2Start) =< min(Range1End, Range2End).


-spec ipv4_to_int(inet:ip4_address()) -> 0..4294967295.
ipv4_to_int({A, B, C, D}) ->
    {_, Int} = lists:foldl(fun(X, {Pos, Acc}) -> {Pos + 1, Acc + (X bsl (Pos * 8))} end, {0, 0}, [D, C, B, A]),
    Int.


-spec int_to_ipv4(0..4294967295) -> inet:ip4_address().
int_to_ipv4(Int) when Int >= 0 andalso Int =< 4294967295 ->
    Byte = 16#ff,
    {(Int bsr 24) band Byte, (Int bsr 16) band Byte, (Int bsr 8) band Byte, Int band Byte}.
