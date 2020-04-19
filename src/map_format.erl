%%%-------------------------------------------------------------------
%%% @author whn
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 2月 2020 2:03 下午
%%%-------------------------------------------------------------------
-module(map_format).
-author("whn").

%% API
-export([make_keys_atom/1]).

make_keys_atom(Map) when is_map(Map) ->
    Fun =
        fun(Key, Val, Acc) ->
            NewKey =
                case is_binary(Key) of
                    true ->
                        binary_to_atom(Key, utf8);
                    _ ->
                        Key
                end,
            Acc#{NewKey => make_keys_atom(Val)}
        end,
    maps:fold(Fun, #{}, Map);
make_keys_atom(List) when is_list(List) ->
    case List of
        [Map | _] when is_map(Map) ->
            [make_keys_atom(X) || X <- List];
        _ ->
            List
    end;
make_keys_atom(Map) -> Map.