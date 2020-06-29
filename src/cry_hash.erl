%% @author Shawn

-module(cry_hash).

%% ====================================================================
%% API functions
%% ====================================================================

-export([md5/1, sha/1, sha256/1, hmacsha256/2, sha512/1, hmacsha512/2, des_encode/2, des_decode/2]).

md5(List) ->
  Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(List))]),
  binary_to_list(Bin).

sha(List) ->
    Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(crypto:hash(sha, List))]),
    binary_to_list(Bin).

sha256(List) ->
    Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(crypto:hash(sha256, List))]),
    binary_to_list(Bin).

hmacsha256(List, Key) ->
    Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(crypto:hmac(sha256, Key, List))]),
    binary_to_list(Bin).

sha512(List) ->
    Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(crypto:hash(sha512, List))]),
    binary_to_list(Bin).

hmacsha512(List, Key) ->
    Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(crypto:hmac(sha512, Key, List))]),
    binary_to_list(Bin).

%% Data=string/binary
des_encode(Data, Key) ->
    ParS = des_plain_text(Data),
    Des = crypto:block_encrypt(des_ecb, Key, ParS),
    base64:encode(Des).

des_decode(Key, Bin) ->
    Binary = crypto:block_decrypt(des_ecb, Key, base64:decode(Bin)),
    List = binary_to_list(Binary),
    L = [Num | _] = lists:reverse(List),
    do_clean_padding(Num, Num, L).

%% ====================================================================
%% Internal functions
%% ====================================================================


des_plain_text(String) when is_list(String) ->
    des_plain_text(list_to_binary(String));
des_plain_text(Data) ->
    N = 8 - (erlang:byte_size(Data) rem 8),
    NewData = erlang:binary_to_list(Data),
    lists:append(NewData, get_padding(N)).


do_clean_padding(0, _, List) -> lists:reverse(List);
do_clean_padding(Num, Val, List) ->
    [_|T] = List,
    do_clean_padding(Num-1,Val,T).

%% DES 规则
get_padding(N) ->
    case N of
        0 ->
            get_padding2(8, 8, []);
        Num ->
            get_padding2(Num, Num, [])
    end.

get_padding2(N, Val, PaddingList) when N > 0 ->
    get_padding2(N-1, Val, [Val] ++ PaddingList);
get_padding2(N, _Val, PaddingList) when N == 0 ->
    PaddingList.

