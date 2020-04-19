%%%-------------------------------------------------------------------
%%% @author ysx
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 浮点数字符串用整型计算
%%% @end
%%% Created : 05. Apr 2020 07:52
%%%-------------------------------------------------------------------
-module(di_math).
-author("ysx").

%% ==================================================
%% API
%% ==================================================
-export([compare/2]).
-export([add/2, subtract/2, multiply/2]).

compare(N1, N2) ->
    {{Symbol1, I1, D1}, {Symbol2, I2, D2}, _Len} = format_as_decimal(N1, N2),
    case {Symbol1, Symbol2} of
        {"", "-"} ->
            case {I1, D1} of
                {I2, D2} ->
                    equal;
                _ ->
                    true
            end;
        {"-", ""} ->
            case {I1, D1} of
                {I2, D2} ->
                    equal;
                _ ->
                    false
            end;
        {"", ""} ->
            compare_split({I1, D1}, {I2, D2});
        _ ->
            R = compare_split({I1, D1}, {I2, D2}),
            case R of
                true ->
                    false;
                false ->
                    true;
                R ->
                    R
            end
    end.

add(N1, N2) ->
    {{Symbol1, I1, D1}, {Symbol2, I2, D2}, Len} = format_as_decimal(N1, N2),
    {Symbol, I, D} =
    case Symbol1 of
        Symbol2 ->
            {TI, TD} = positive_add({I1, D1}, {I2, D2}, Len),
            {Symbol1, TI, TD};
        _ ->
            case compare_split({I1, D1}, {I2, D2}) of
                true ->
                    {TI, TD} = positive_subtract({I1, D1}, {I2, D2}, Len),
                    {Symbol1, TI, TD};
                false ->
                    {TI, TD} = positive_subtract({I2, D2}, {I1, D1}, Len),
                    {Symbol2, TI, TD};
                equal ->
                    {"", 0, "0"}
            end
    end,
    to_string(Symbol, I, D).


subtract(N1, N2) ->
    case N1 of
        N2 ->
            "0";
        _ ->
            [H|T] = N2,
            case H of
                $- ->
                    add(N1, T);
                _ ->
                    add(N1, [$-|N2])
            end
    end.


multiply(N1, N2) ->
    {{Symbol1, I1, D1, Len1}, {Symbol2, I2, D2, Len2}} = format_decimal(N1, N2),
    case {I1, D1, I2, D2} of
        {0, 0, _, _} ->
            "0";
        {_, _, 0, 0} ->
            "0";
        _ ->
            {IR1, DR1} = decimal_multiply(I1, D2, Len2),
            {IR2, DR2} = decimal_multiply(I2, D1, Len1),
            {_IR3, DR3} = decimal_multiply(D1, D2, Len1+Len2),
            I = I1 * I2 + IR1 + IR2,
            D = add_multiply_decimal_part(DR1, DR2, DR3),
            Symbol =
                case Symbol1 of
                    Symbol2 ->
                        "";
                    _ ->
                        "-"
                end,
            to_string(Symbol, I, D)
    end.



%% ==================================================
%% Internal
%% ==================================================

split_decimal(D) ->
    split_decimal(D, [], [], "").

split_decimal([], I, D, Symbol) ->
    I1 = lists:reverse(I),
    case D of
        [] ->
            {Symbol, I1, "0"};
        _ ->
            {Symbol, I1, D}
    end;
split_decimal([H|T], I, D, Symbol) ->
    case H of
        $- ->
            split_decimal(T, I, D, "-");
        $. ->
            split_decimal([], I, T, Symbol);
        _ ->
            split_decimal(T, [H|I], D, Symbol)
    end.

format_as_decimal(N1, N2) ->
    {{Symbol1, II1, DI1, Len1}, {Symbol2, II2, DI2, Len2}} = format_decimal(N1, N2),
    {{DD1, DD2}, Len} =
    case Len1 of
        Len2 ->
            {{DI1, DI2}, Len1};
        _ when Len1 > Len2  ->
            {{DI1, pow(DI2, Len1 - Len2)}, Len1};
        _ ->
            {{pow(DI1, Len2 - Len1), DI2}, Len2}
    end,
    {{Symbol1, II1, DD1}, {Symbol2, II2, DD2}, Len}.

format_decimal(N1, N2) ->
    {Symbol1, I1, D1} = split_decimal(N1),
    {Symbol2, I2, D2} = split_decimal(N2),
    Len1 = length(D1),
    Len2 = length(D2),
    DD1 = list_to_integer(D1),
    DD2 = list_to_integer(D2),

    II1 = list_to_integer(I1),
    II2 = list_to_integer(I2),

    {{Symbol1, II1, DD1, Len1}, {Symbol2, II2, DD2, Len2}}.


to_string(Symbol, I, D) ->
    case D of
        "0" ->
            lists:concat([Symbol, I]);
        _ ->
            lists:concat([Symbol, I, ".", D])
    end.

pow(X, 0) ->
    X;
pow(X, Y) ->
    pow(X*10, Y-1).

decimal_part_add(D1, D2, Len) ->
    S = D1 + D2,
    Str = integer_to_list(S),
    LenS = length(Str),
    case LenS of
        Len ->
            {Str, 0};
        _ when LenS < Len ->
            Str0 = add_front_zero(Str, Len - LenS),
            {Str0, 0};
        _ ->
            [_|T] = Str,
            {T, 1}
    end.

decimal_part_subtract(D1, D2, Len) ->
    S = D1 - D2,
    case S < 0 of
        true ->
            {integer_to_list(pow(1, Len) + D1 - D2), 1};
        _ ->
            Str = integer_to_list(S),
            LenS = length(Str),
            Str0 =
            case LenS < Len of
                true ->
                    add_front_zero(Str, Len - LenS);
                false ->
                    Str
            end,
            {Str0, 0}
    end.

decimal_multiply(A, B, Len) ->
    case {A, B} of
        {0, _} ->
            {0, "0"};
        {_, 0} ->
            {0, "0"};
        _ ->
            S = A * B,
            Str = integer_to_list(S),
            LenS = length(Str),
            Str0 =
                case LenS < Len of
                    true ->
                        add_front_zero(Str, Len-LenS);
                    false ->
                        Str
                end,
            {IR, DR} = lists:split(length(Str0) - Len, Str0),
            case IR of
                [] ->
                    {0, DR};
                _ ->
                    {list_to_integer(IR), DR}
            end
    end.

add_multiply_decimal_part(D1, D2, D3) ->
    N1 = format_md(D1),
    N2 = format_md(D2),
    N3 = format_md(D3),
    T = add(N1,N2),
    TT = add(T, N3),
    case TT of
        "0" ->
            TT;
        _ ->
            [_, _|Tail] = TT,
            Tail
    end.


format_md(MD) ->
    [$0|[$.|MD]].

%%decimal_decimal_multiply(D1, D2, Len1, Len2) ->
%%    lager:info("D1:~p, D2:~p", [D1, D2]),
%%    lager:info("Len1:~p, Len2:~p", [Len1, Len2]),
%%    TotalLen = Len1 + Len2,
%%    case {D1, D2} of
%%        {0, _} ->
%%            {0, "0"};
%%        {_, 0} ->
%%            {0, "0"};
%%        _ ->
%%            S = D1 * D2,
%%            Str = integer_to_list(S),
%%            LenS = length(Str),
%%            Str0 =
%%                case LenS < TotalLen of
%%                    true ->
%%                        add_front_zero(Str, TotalLen - LenS);
%%                    false ->
%%                        Str
%%                end,
%%            {IR, DR} = lists:split(length(Str0) - TotalLen, Str0),
%%            case IR of
%%                [] ->
%%                    {0, DR};
%%                _ ->
%%                    {list_to_integer(IR), DR}
%%            end
%%    end.



add_front_zero(Str, 0) ->
    Str;
add_front_zero(Str, N) ->
    add_front_zero([$0|Str], N-1).

compare_split({I1, D1}, {I2, D2}) ->
    case I1 of
        I2 ->
            case D1 of
                D2 ->
                    equal;
                _ when I1 > 0 ->
                    D1 > D2;
                _ ->
                    D1 < D2
            end;
        _ when I1 > I2 ->
            true;
        _ ->
            false
    end.

positive_add({I1, D1}, {I2, D2}, Len) ->
    {D, IPlus} = decimal_part_add(D1, D2, Len),
    I = I1 + I2 + IPlus,
    {I, D}.

positive_subtract({I1, D1}, {I2, D2}, Len) ->
    {D, IPlus} = decimal_part_subtract(D1, D2, Len),
    I = I1 - I2 - IPlus,
    {I, D}.



