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
-export([des_ecb_encode/2, des_cbc_encode/2, des_ecb_decode/2, des_cbc_decode/2, to_hex/1, get_padding/1, to_bin/1]).


des_ecb_encode(Key, Data) ->
    ParS1 = des_plain_text(Data),
    Bin = crypto:block_encrypt(des_ecb, Key, ParS1),
    Bin.
%%    binary_to_list(to_hex(Bin)).
des_cbc_encode(Key, Data) ->
    ParS1 = des_plain_text(Data),
    Bin = crypto:block_encrypt(des_cbc, Key, <<0:64>>, ParS1),
    binary_to_list(to_hex(Bin)).

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




%% @spec to_hex(integer | iolist()) -> string()
%% @doc Convert an iolist to a hexadecimal string.
to_hex(0) ->
    "0";
to_hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
to_hex(B) ->
    to_hex(iolist_to_binary(B), []).

%% @spec to_bin(string()) -> binary()
%% @doc Convert a hexadecimal string to a binary.
to_bin(L) ->
    to_bin(L, []).

%% @spec to_int(string()) -> integer()
%% @doc Convert a hexadecimal string to an integer.
to_int(L) ->
    erlang:list_to_integer(L, 16).

%% @spec dehex(char()) -> integer()
%% @doc Convert a hex digit to its integer value.
dehex(C) when C >= $0, C =< $9 ->
    C - $0;
dehex(C) when C >= $a, C =< $f ->
    C - $a + 10;
dehex(C) when C >= $A, C =< $F ->
    C - $A + 10.

%% @spec hexdigit(integer()) -> char()
%% @doc Convert an integer less than 16 to a hex digit.
hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

%% Internal API

to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

to_hex_int(0, Acc) ->
    Acc;
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).

to_bin([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
    to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).




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