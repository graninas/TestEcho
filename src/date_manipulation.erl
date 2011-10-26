%%%-------------------------------------------------------------------
%%% File    : date_manipulation.erl
%%% Author  : Granin Alexandr <graninas@gmail.com>
%%% Description : Some date manipulation functions needed in data_collection module.
%%%-------------------------------------------------------------------

-module(date_manipulation).
-export([scale_date/2, add_to_datetime/2]).
-export([minute_begin/1, hour_begin/1, day_begin/1, month_begin/1, year_begin/1]).


%%====================================================================
%% Date manipulation API.
%%====================================================================

%%--------------------------------------------------------------------
%% Function: scale_date(DateTime, Scale) -> ScaledDateTime
%% Description: Returns begin and DateTimeDiff of the period.
%%
%% Scale = minute | hour | day | week | month | year
%% ScaledDateTime = {DateTime, DateTimeDiff}
%% DateTimeDiff = {DiffScale, DiffValue}
%% DiffScale = seconds | months | years
%% DiffValue = Int
%%--------------------------------------------------------------------
scale_date(DateTime, minute) -> {minute_begin(DateTime), {seconds, 60}};
scale_date(DateTime, hour)   -> {hour_begin(DateTime),   {seconds, 60 * 60}};
scale_date(DateTime, day)    -> {day_begin(DateTime),    {seconds, 60 * 60 * 24}};
scale_date(DateTime, week)   -> {day_begin(DateTime),    {seconds, 60 * 60 * 24 * 7}};
scale_date(DateTime, month)  -> {month_begin(DateTime),  {months,  1}};
scale_date(DateTime, year)   -> {year_begin(DateTime),   {years,   1}}.

%%--------------------------------------------------------------------
%% Function: add_to_datetime(DateTimeDiff, DateTime) -> DateTime
%% Description: Adds DateTimeDiff to the DateTime and returns result.
%%
%% DateTimeDiff = {DiffScale, DiffValue}
%% DiffScale = seconds | months | years
%% DiffValue = Int
%%--------------------------------------------------------------------
%% Needs to be corrected in case {months, Val} when Val > 1.
add_to_datetime({seconds, Val}, DateTime) -> calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime) + Val);
add_to_datetime({months, Val}, {{Year, Month, Day}, Time}) ->
    DaysCount = calendar:last_day_of_the_month(Year, Month),	%% all months have different days count.
    SecondsCount = DaysCount * (60 * 60 * 24) * Val,
    DateTime = {{Year, Month, Day}, Time},
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime) + SecondsCount);
add_to_datetime({years, Val}, {{Year, Month, Day}, Time}) ->
	{{Year + Val, Month, Day}, Time}.

%%--------------------------------------------------------------------
%% Functions: xxx_begin(DateTime) -> DateTime
%% Return appropriate period begin.
%%--------------------------------------------------------------------
minute_begin({Date, {Hour, Minute, _}}) -> {Date, {Hour, Minute, 0}}.
hour_begin({Date, {Hour, _, _}})        -> {Date, {Hour, 0, 0}}.
day_begin({Date, _})                    -> {Date, {0, 0, 0}}.
month_begin({{Year, Month, _}, _})      -> {{Year, Month, 1}, {0, 0, 0}}.
year_begin({{Year, _, _}, _})           -> {{Year, 1, 1}, {0, 0, 0}}.

