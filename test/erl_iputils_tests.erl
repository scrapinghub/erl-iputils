-module(erl_iputils_tests).

-include_lib("triq/include/triq.hrl").
-include_lib("eunit/include/eunit.hrl").


prop_ipv4_to_int() ->
    ?FORALL(
        {Int, Ip},
        int_ipv4(),
        begin
            Int == erl_iputils:ipv4_to_int(Ip)
        end
    ).


prop_int_to_ipv4() ->
    ?FORALL(
        {Int, Ip},
        int_ipv4(),
        begin
            Ip == erl_iputils:int_to_ipv4(Int)
        end
    ).


prop_parse_range() ->
    ?FORALL(
        {Base, Subnet, Range},
        string_range(),
        begin
            {ok, {Start, End}} = erl_iputils:parse_range(Range),
            Shift = 32 - Subnet,
            Start1 = (Base bsr Shift) bsl Shift,
            End1 = lists:foldl(fun(Shft, Acc) -> Acc bor (1 bsl Shft) end, Base, lists:seq(0, Shift - 1)),
            Start == Start1 andalso End == End1
        end
    ).


prop_ranges_overlap() ->
    ?FORALL(
        {{_, _, Range1}, {_, _, Range2}},
        {string_range(), string_range()},
        begin
            {ok, {S1, E1}} = erl_iputils:parse_range(Range1),
            {ok, {S2, E2}} = erl_iputils:parse_range(Range2),
            case erl_iputils:ranges_overlap({S1, E1}, {S2, E2}) == (
                ((S1 =< S2 andalso S2 =< E1) orelse (S1 =< E2 andalso E2 =< E1) orelse
                 (S2 =< S1 andalso S1 =< E2))
            ) of 
                true -> true;
                false -> ?debugFmt("~s - ~s~n", [Range1, Range2]), false
            end
        end
    ).


int_ipv4() ->
    ?LET(
        RawInt,
        triq_dom:int(0, 16#ffffffff),
        begin
            Int = abs(RawInt) band 16#ffffffff,
            Byte = 16#ff,
            Ip = {(Int bsr 24) band Byte, (Int bsr 16) band Byte, (Int bsr 8) band Byte, Int band Byte},
            {Int, Ip}
        end
    ).


string_range() ->
    ?LET(
        {Base, Subnet},
        {triq_dom:int(0, 16#ffffffff), triq_dom:int(0, 32)},
        begin
            MaybeToBinary = fun(String) ->
                {_, _, X} = os:timestamp(),
                case X rem 2 of
                    0 -> list_to_binary(String);
                    1 -> String
                end
            end,
            Range = inet:ntoa(erl_iputils:int_to_ipv4(Base)) ++ "/" ++ integer_to_list(Subnet),
            {Base, Subnet, MaybeToBinary(Range)}
        end
    ).


iputils_property_test_() ->
    {timeout, 1500, fun() -> true = check() end}.
