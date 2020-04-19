%%%-------------------------------------------------------------------
%%% @author whn
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 4月 2020 3:45 下午
%%%-------------------------------------------------------------------
-module(regex).
-author("whn").

%% API
-export([verify_id_card/1, verify/2]).

%% 根据计算方法验证身份证号码真伪
verify_id_card(IDCard) when is_list(IDCard) ->
    case catch lists:sublist(IDCard, 17) of
        List when is_list(List) ->
            [H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12,H13,H14,H15,H16,H17] = List,
            Value = list_to_integer([H1]) * 7+
            list_to_integer([H2]) * 9+
            list_to_integer([H3]) * 10+
            list_to_integer([H4]) * 5+
            list_to_integer([H5]) * 8+
            list_to_integer([H6]) * 4+
            list_to_integer([H7]) * 2+
            list_to_integer([H8]) * 1+
            list_to_integer([H9]) * 6+
            list_to_integer([H10]) * 3+
            list_to_integer([H11]) * 7+
            list_to_integer([H12]) * 9+
            list_to_integer([H13]) * 10+
            list_to_integer([H14]) * 5+
            list_to_integer([H15]) * 8+
            list_to_integer([H16]) * 4+
            list_to_integer([H17]) * 2,
            Tail =
                case Value rem 11 of
                    0 -> "1";
                    1 -> "0";
                    2 -> "X";
                    3 -> "9";
                    4 -> "8";
                    5 -> "7";
                    6 -> "6";
                    7 -> "5";
                    8 -> "4";
                    9 -> "3";
                    10 -> "2"
                end,
            case lists:sublist(IDCard, 18, 1) of
                "x" ->
                    "X" =:= Tail;
                Tail ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end;
verify_id_card(IDCard) ->
    verify_id_card(unicode:characters_to_list(IDCard)).


verify(Str, Value) ->
    {ok, RE} = re:compile(Str),
    re:run(Value, RE).