-module(web_lib).
-export([
    parse_query/1
    ,escape_uri/1
]).


%% 解析 QueryString
%% -> [{key::string(), value::string()}]
parse_query(String) when is_binary(String) ->
    parse_query(binary_to_list(String));
parse_query(String) ->
    parse_query(String, "&", "=").
parse_query(String, Token1, Token2) when is_bitstring(String) ->
    parse_query(binary_to_list(String), Token1, Token2);
parse_query(String, Token1, Token2) ->
    [ list_to_tuple(string:tokens(KV, Token2)) || KV <- string:tokens(String, Token1) ].


%% 类似urlencode()
escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".

escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].

