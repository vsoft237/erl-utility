%%----------------------------------------------------
%% @doc  时间函数
%% 
%% @author QingXuan 
%% @end
%%---------------------------------------------------- 
-module(time).

-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_DAY_SECONDS,86400).

-export([now/0, now/1, mktime/1,
		 is_same_date/2, is_same_month/2, is_same_date_by_login/1,
		 seconds_to_localtime/1, is_week_date/1,
		 format_seconds/1, add_days_to_localtime/2,seconds_to_string/1,
	seconds_to_binary/1
]).

-export([get_midnight_seconds/1, get_localtime/0,
		 get_localtime_string/0, date_num/0,
		 get_surplus_weekday/0,
		 get_weekday/0, get_localtime_raw_string/0, get_today_seconds/0
		]).

-export([get_huobi_time/0, next_diff/1, next_diff/3]).

%% 取得当前unix时间戳，精确到秒
now() ->
	{M, S, _} = erlang:timestamp(),
	M * 1000000 + S.

%% 取得当前unix时间戳，精确到微秒
now(micro) ->
	{M, S, Micro} = erlang:timestamp(),
	M * 1000000000000 + S * 1000000 + Micro;

%% 取得当前unix时间戳，精确到毫秒
now(milli_second) ->
	{M, S, Micro} = erlang:timestamp(),
	M * 1000000000 + S * 1000 + Micro div 1000.

%% 生成一个指定日期的unix时间戳（无时区问题）
mktime({Date, Time}) ->
	%% 	DATETIME 
	%% 一个日期和时间组合。支持的范围是'1000-01-01 00:00:00'到'9999-12-31 23:59:59'。MySQL以'YYYY-MM-DD HH:MM:SS'格式来显示DATETIME值，
	%% 但是允许你使用字符串或数字把值赋给DATETIME的列。
	DT = erlang:localtime_to_universaltime({Date, Time}),
	calendar:datetime_to_gregorian_seconds(DT) - ?DIFF_SECONDS_0000_1900.

%% 判断是否同一天
%% -----------------------------------------------------------------
is_same_date(Seconds1, Seconds2) ->
	{{Year1, Month1, Day1}, _Time1} = seconds_to_localtime(Seconds1),
	{{Year2, Month2, Day2}, _Time2} = seconds_to_localtime(Seconds2),
	if ((Year1 /= Year2) or (Month1 /= Month2) or (Day1 /= Day2)) -> false;
	   true -> true
	end.

%% 判断是否同一个月
%% -----------------------------------------------------------------
is_same_month(Seconds1, Seconds2) ->
	{{Year1, Month1, _Day1}, _Time1} = seconds_to_localtime(Seconds1),
	{{Year2, Month2, _Day2}, _Time2} = seconds_to_localtime(Seconds2),
	if ((Year1 /= Year2) or (Month1 /= Month2)) -> false;
	   true -> true
	end.

is_same_date_by_login(LogoutTime) ->
	{datetime, {{Year1, Month1, Day1}, _}} = LogoutTime,
	{{Year2, Month2, Day2}, _} = time:get_localtime(),
	% 判断是否同一天
	if ((Year1 /= Year2) or (Month1 /= Month2) or (Day1 /= Day2)) -> false;
	   true -> true
	end.

is_week_date(Seconds) ->
	{Date, _} = seconds_to_localtime(Seconds),
	calendar:day_of_the_week(Date).

seconds_to_localtime(Seconds) ->
	DateTime = calendar:gregorian_seconds_to_datetime(Seconds+?DIFF_SECONDS_0000_1900),
	calendar:universal_time_to_local_time(DateTime).

add_days_to_localtime({Year, Month, Day}, AddDays) ->
	Days = calendar:date_to_gregorian_days(Year, Month, Day),
	calendar:gregorian_days_to_date(Days + AddDays).

get_localtime() ->
	Seconds = time:now(),
	DateTime = calendar:gregorian_seconds_to_datetime(Seconds+?DIFF_SECONDS_0000_1900),
	calendar:universal_time_to_local_time(DateTime).

get_localtime_string() ->
	%% DATETIME
	%% 一个日期和时间组合。支持的范围是'1000-01-01 00:00:00'到'9999-12-31 23:59:59'。MySQL以'YYYY-MM-DD HH:MM:SS'格式来显示DATETIME值，
	%% 但是允许你使用字符串或数字把值赋给DATETIME的列。
	{{Year, Month, Day}, {Hour, Minute, Second}} = time:get_localtime(),
	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour, Minute, Second])).

seconds_to_binary(undefined) -> <<"NULL">>;
seconds_to_binary(null) -> <<"NULL">>;
seconds_to_binary(<<>>) -> <<"NULL">>;
seconds_to_binary({{Year, Month, Day}, {Hour, Minute, Second}}) ->
	%% DATETIME
	%% 一个日期和时间组合。支持的范围是'1000-01-01 00:00:00'到'9999-12-31 23:59:59'。MySQL以'YYYY-MM-DD HH:MM:SS'格式来显示DATETIME值，
	%% 但是允许你使用字符串或数字把值赋给DATETIME的列。
	list_to_binary(lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour, Minute, Second]))).

seconds_to_string(undefined) -> <<"NULL">>;
seconds_to_string(null) -> <<"NULL">>;
seconds_to_string(<<>>) -> <<"NULL">>;
seconds_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
	%% DATETIME
	%% 一个日期和时间组合。支持的范围是'1000-01-01 00:00:00'到'9999-12-31 23:59:59'。MySQL以'YYYY-MM-DD HH:MM:SS'格式来显示DATETIME值，
	%% 但是允许你使用字符串或数字把值赋给DATETIME的列。
	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour, Minute, Second])).


get_huobi_time() ->
	%% DATETIME
	%% 一个日期和时间组合。支持的范围是'1000-01-01 00:00:00'到'9999-12-31 23:59:59'。MySQL以'YYYY-MM-DD HH:MM:SS'格式来显示DATETIME值，
	%% 但是允许你使用字符串或数字把值赋给DATETIME的列。
	{{Year, Month, Day}, {Hour, Minute, Second}} = time:get_localtime(),
	TS = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour, Minute, Second])),
	http_uri:encode(TS).

get_localtime_raw_string() ->
	%% DATETIME
	%% YYYYMMDDHHMMSS'格式来显示DATETIME值，
	{{Year, Month, Day}, {Hour, Minute, Second}} = time:get_localtime(),
	lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w",[Year, Month, Day, Hour, Minute, Second])).

%% 得到现在日期--20120630
date_num() ->
	{{Y, M, D}, _} = get_localtime(),
	Y * 10000 + M * 100 + D.

%% 返回XX分：XX秒格式字符串
format_seconds(Seconds) ->
	case Seconds > 60 of
		true ->
			integer_to_list(Seconds div 60) ++ "分" ++ integer_to_list(Seconds rem 60) ++ "秒";
		false ->
			integer_to_list(Seconds) ++ "秒"
	end.

get_weekday() ->
	{{Year, Month, Day}, _} = time:get_localtime(),
	calendar:day_of_the_week({Year, Month, Day}).

%% 本星期剩余多少天多少小时
%% -----------------------------------------------------------------
get_surplus_weekday() ->
	{{Year, Month, Day}, {Hour, _, _}} = time:get_localtime(),
	WeekDay = calendar:day_of_the_week({Year, Month, Day}),
	SurplusDay = 7 - WeekDay,
	SurplusHour = 24 - Hour,
	% unicode:characters_to_list(Str, utf8).
	List = integer_to_list(SurplusDay) ++ "天" ++ integer_to_list(SurplusHour) ++ "小时",
	transform:to_binary(List).

%%获取今天的24:00:00 和今天的）00:00:00的秒数
get_midnight_seconds(Seconds) ->
	{{_Year, _Month, _Day}, Time} = seconds_to_localtime(Seconds),
	% 从午夜到现在的秒数
	Diff   = calendar:time_to_seconds(Time),
	% 获取当天0点
	Today  = Seconds - Diff,
	% 获取第二天0点
	NextDay = Seconds + (?ONE_DAY_SECONDS-Diff),
	{Today, NextDay}.


get_today_seconds() ->
	calendar:time_to_seconds(time()).

%% 获取今天特定时间戳
%%get_today_spec_time({H, M, S}) ->
%%	{TZ, _} = get_midnight_seconds(time:now()),
%%	ok.


%% @doc 取得当前距离指定时间下次到达时相差的秒数
-spec next_diff(H, M, S) -> Seconds when
	H :: 0..23,
	M :: 0..59,
	S :: 0..59,
	Seconds :: pos_integer().
next_diff(H, M, S) ->
	Sec = H * 3600 + M * 60 + S,
	next_diff(Sec).
-spec next_diff(0..86400 | [0..86400]) -> Seconds::pos_integer().
next_diff(L = [_ | _]) ->
	lists:min([next_diff(Sec) || Sec <- L]);
next_diff({H, M, S}) ->
	next_diff(H, M, S);
next_diff(Sec) ->
	%% Now = datetime(),
	%% next_diff(Sec, Now).
	DaySec = calendar:time_to_seconds(time()),
	case Sec > DaySec of
		true -> Sec - DaySec;
		false -> Sec + 86400 - DaySec
	end.
