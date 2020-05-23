%%%-------------------------------------------------------------------
%%% @author whn
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 2月 2020 3:35 下午
%%%-------------------------------------------------------------------
-module(json).
-author("whn").

%% API
-export([encode/1, decode/1]).

encode(Map) ->
    jsx:encode(Map).

decode(String) when is_list(String) ->
    decode(list_to_binary(String));
decode(Json) ->
    case catch jsx:decode(Json, [return_maps]) of
        Map when is_map(Map) ->
            {ok, Map};
        _ErrorMsg ->
            {error, 1}
    end.