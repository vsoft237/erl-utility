%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jan 2016 2:09 PM
%%%-------------------------------------------------------------------
-module(sdk).
-author("ysx").

%% API
-export([http/3]).

-export([format_http_get_params/1, format_http_post_params/1]).

http(Method, Url, Params) ->
    case http_request(Method, Url, Params) of
        {ok, {{_Version, _, _ReasonPhrase}, _ReturnHeaders, Result}} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

format_http_get_params(PropList) ->
    string:join([atom_to_list(Key) ++ "=" ++ format_val(Val) || {Key, Val} <- PropList], "&").

format_http_post_params(PropList) ->
    string:join([atom_to_list(Key) ++ "=" ++ format_val(Val) || {Key, Val} <- PropList], ";").


%% Internal
http_request(Method, Url, Params) ->
    case Method of
        get ->
            Arg = Url ++ "?" ++ Params,
            http_get(Arg);
        post ->
            http_post(Url, Params);
        post_json ->
            http_post_json(Url, Params)
    end.

http_get(Arg) ->
    Headers = [
        {"charset", "utf-8"},
        {"content-type", "text/html"}],
    httpc:request(get, {Arg, Headers}, [{timeout, 5000}], []).

http_post(Url, Body) ->
    Headers = [{"content-type", "ext/xml;charset=utf-8"}],
    ContentType = "ext/xml;charset=utf-8",
    httpc:request(post, {Url, Headers, ContentType, Body}, [{timeout, 5000}], []).


http_post_json(Url, Body) ->
    Headers = [{"content-type", "ext/xml;charset=utf-8"}],
    ContentType = "application/json;charset=utf-8",
    httpc:request(post, {Url, Headers, ContentType, Body}, [{timeout, 5000}], []).

format_val(Val) when is_list(Val) ->
    Val;
format_val(Val) when is_binary(Val) ->
    binary_to_list(Val);
format_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
format_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
format_val(Val) when is_float(Val) ->
    float_to_list(Val).

