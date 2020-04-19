%%%-------------------------------------------------------------------
%%% @author whn
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 3月 2020 5:31 下午
%%%-------------------------------------------------------------------
-module(cry_des).
-author("whn").

%% API
-export([des_ecb_encode/2, des_cbc_encode/2, des_ecb_decode/2, des_cbc_decode/2, to_2/1, to_16/1, get_padding/1]).


des_ecb_encode(Key, Data) ->
    ParS1 = des_plain_text(Data),
    Bin = crypto:block_encrypt(des_ecb, Key, ParS1),
    Bin.
%%    binary_to_list(to_16(Bin)).
des_cbc_encode(Key, Data) ->
    ParS1 = des_plain_text(Data),
    Bin = crypto:block_encrypt(des_cbc, Key, <<0:64>>, ParS1),
    binary_to_list(to_16(Bin)).

des_ecb_decode(Key, Bin) ->
%%    Bin = base64:decode(Data),
    Binary = crypto:block_decrypt(des_ecb, Key, Bin),
    List = binary_to_list(Binary),
    L = [Num | _] = lists:reverse(List),
    do_clean_padding(Num, Num, L).


des_cbc_decode(Key, Bin) ->
    Binary = crypto:block_decrypt(des_cbc, Key, <<0:64>>, Bin),
    List = binary_to_list(Binary),
    L = [Num | _] = lists:reverse(List),
    do_clean_padding(Num, Num, L).



to_16(List) ->
    list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(List)]).

to_2(List) ->
    to_2(List, []).
to_2([], R) ->
    erlang:list_to_binary(lists:reverse(R));
to_2(Binary, R) when is_binary(Binary)->
    to_2(erlang:binary_to_list(Binary), R);
to_2(List, R) ->
    A = lists:sublist(List,2),
    NewList = List--A,
    NewR = [erlang:list_to_integer(A,16) | R],
    to_2(NewList, NewR).



des_plain_text(String) when is_list(String) ->
    des_plain_text(list_to_binary(String));
des_plain_text(Data) ->
    N = 8 - (erlang:byte_size(Data) rem 8),
    NewData = erlang:binary_to_list(Data),
    lists:append(NewData, get_padding(N)).

%%
%%    Length = length(Text),
%%    Len = 8 - Length rem 8,
%%    case Len of
%%        8 ->
%%            Text;
%%        _ ->
%%            Tail = lists:map(fun(_) ->
%%                2
%%            end, lists:seq(1,Len)),
%%            Text ++ Tail
%%    end.



%% DES 规则
get_padding(N) ->
    case N of
        0 ->
            get_padding2(8,8,[]);
        Num ->
            get_padding2(Num,Num,[])
    end.

get_padding2(N, Val, PaddingList) when N > 0 ->
    get_padding2(N-1, Val, [Val] ++ PaddingList);
get_padding2(N, _Val,PaddingList) when N == 0 ->
    PaddingList.



do_clean_padding(0, _, List) -> lists:reverse(List);
do_clean_padding(Num, Val, List) ->
    [_|T] = List,
    do_clean_padding(Num-1,Val,T).