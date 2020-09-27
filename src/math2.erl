%% @author Administrator
%% @doc @todo Add description to math2.


-module(math2).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	ceil/1,
	floor/1,
	zero_valid/1,
	one_valid/1,
	any_to_num/1,
	float_to_string/2,
	float_to_bin/2
]).

%% 返回大于于或者等于指定表达式的最大整数
ceil(Value) ->
	Value1 = erlang:round(Value),
	case Value1 >= Value of
		true -> Value1;
		false -> Value1 + 1
	end.

%% 返回小于或者等于指定表达式的最大整数
floor(Value) ->
	Value1 = erlang:round(Value),
	case Value1 =< Value of
		true -> Value1;
		false -> Value1 - 1
	end.

%% 返回不小于0的数值
zero_valid(Value) ->
	case Value < 0 of
		true -> 0;
		false -> Value
	end.

%% 若计算得值小于1则默认为1
one_valid(Value) ->
	case Value < 1 of
		true ->
			1;
		false ->
			% math2:ceil(Value) 
			erlang:round(Value)
	end.

any_to_num(<<>>) -> 0;
any_to_num("") -> 0;
any_to_num(Binary) when is_binary(Binary) ->
	case catch binary_to_integer(Binary) of
		Int when is_integer(Int) ->
			Int;
		_ ->
			case catch binary_to_float(Binary) of
				Float when is_float(Float) ->
					Float;
				_ ->
					string_to_num(binary_to_list(Binary))
			end
	end;

any_to_num(String) when is_list(String) ->
	case catch list_to_integer(String) of
		R when is_integer(R) ->
			R;
		_ ->
			case catch list_to_float(String) of
				Float when is_float(Float) ->
					Float;
				_ ->
					string_to_num(String)
			end
	end;
any_to_num(Integer) when is_integer(Integer) ->
	Integer;
any_to_num(Float) when is_float(Float) ->
	Float;
any_to_num(_) ->
%%	io:format("Any to num is error when Any is :~p~n", [Any]),
	0.


string_to_num(Str) ->
	{ok, Tokens, _} = erl_scan:string(Str),
	case Tokens of
		[{float, _, Float} | _] ->
			Float;
		[{integer, _, Int} | _] ->
			Int;
		_ ->
			0
	end.

%% 浮点数转字符串 
float_to_string(Value, Decimals) when is_integer(Value) ->
	float_to_list(Value/1, [{decimals, Decimals}]);
float_to_string(Value, Decimals) when is_float(Value) ->
	float_to_list(Value, [{decimals, Decimals}]).

float_to_bin(Value, Decimals) when is_integer(Value) ->
	float_to_binary(Value/1, [{decimals, Decimals}]);
float_to_bin(Value, Decimals) when is_float(Value) ->
	float_to_binary(Value, [{decimals, Decimals}]).

%% ====================================================================
%% Internal functions
%% ====================================================================


