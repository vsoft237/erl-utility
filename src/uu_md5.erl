%% @author Shawn
%% @doc @todo Add description to u_md5.


-module(uu_md5).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  encode/1
  ,des_encode/2
  ,des_decode/2
  ,to_16/1
  ,to_2/1
]).

encode(List) ->	
  Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(List))]),
  binary_to_list(Bin).

%% @doc DES ECB Key::binary(), Data::string()/binary() Return::string()
des_encode(Key, Data) when is_list(Data)->
  N = 8 - (erlang:byte_size(erlang:list_to_binary(Data)) rem 8),
  PlainText = lists:append(Data, get_padding(N)),
  Length = erlang:length(PlainText),
  io:format("Length:~p", [Length]),
  do_des(Key, Length, 8, PlainText, []);
des_encode(Key, Data) when is_binary(Data) ->
  N = 8 - (erlang:byte_size(Data) rem 8),
  NewData = erlang:binary_to_list(Data),
  PlainText = lists:append(NewData, get_padding(N)),
  Length = erlang:length(PlainText),
  do_des(Key, Length, 8, PlainText, []);
des_encode(_Key, _Data) ->
  "".

%% @doc Key::binary() Data::string() 视情况而定，需要url编码解码的 则http_uri:decode一下，不需要的就直接base64:decode
des_decode(Key, Data) ->
  Bin = base64:decode(http_uri:decode(Data)),
  crypto:block_decrypt(des_ecb, Key, Bin).


to_16(List) ->
  list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(unicode:characters_to_binary(List))]).

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

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 每8位加密1次 循环加密 最后按顺序把加密数据串起来
do_des(_, _, _, [], R) -> R;
do_des(Key, Length, Num, List, R) when Length >= Num ->
  {Data, T} = lists:split(Num, List),
  NewData = crypto:block_encrypt(des_ecb, Key, Data),
  do_des(Key, Length-Num, Num, T, R++erlang:binary_to_list(NewData));
do_des(Key, Length, Num, List, R) ->
  io:format("============~p=============", [111111111111111111]),
  NewList = check_des_list(List),
  NewData = crypto:block_encrypt(des_ecb, Key, NewList),
  do_des(Key, Length-Num, Num, [], R++erlang:binary_to_list(NewData)).
check_des_list(List) ->
  N = 8-erlang:length(List),
  L = [0 || _X <- lists:seq(1,N)],
  List ++L.

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
