%%%-------------------------------------------------------------------
%%% File    : data_collection.erl
%%% Author  : Granin Alexandr <graninas@gmail.com>
%%% Description : Data filtering and collecting functions.
%%%-------------------------------------------------------------------

-module(data_collection).
-export([fetch_data/2, filter_data/2, collect/2, test/1, test_echo/0]).

-import(date_manipulation, [scale_date/2, add_to_datetime/2]).

-import(test_data, [test_data_assoc_list/0, test_data_dict/0]).

%%====================================================================
%% Data manipulation API.
%%====================================================================

%%--------------------------------------------------------------------
%% Function: fetch_data(DataDict, Filter) -> ResultList
%% Description: Filter data by Filter, collects data
%% and returns collected ResultList.
%%
%% DataList = [{DateTime, {SecurityName, Price, Amount}}]
%% DataDict = dict:from_list(DataList)
%% Filter = {SecurityName, {DateTimeBegin, DateTimeEnd}, Scale}
%% Scale = minute | hour | day | week | month | year
%% ResultList = [{StartDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount}]
%%--------------------------------------------------------------------
fetch_data(DataDict, Filter) ->
	ResultList = dict:to_list(filter_data(DataDict, Filter)),
	{_, _, Scale} = Filter,
	collect(Scale, ResultList).

%%--------------------------------------------------------------------
%% Function: filter_data(DataDict, Filter) -> DataDict
%% Description: Filters data by Filter predicate. Scale is unused.
%%
%% DataDict = dict:from_list(DataList)
%% DataList = [{DateTime, {SecurityName, Price, Amount}}]
%% Filter = {SecurityName, {DateTimeBegin, DateTimeEnd}, _Scale}
%%--------------------------------------------------------------------
filter_data(DataDict, Filter) ->
	{SecurityName, {DateBegin, DateEnd}, _} = Filter,
	F = fun(K, {Security, _, _}) ->
			(K >= DateBegin) and (K =< DateEnd)
			and (Security == SecurityName)
		end,
	dict:filter(F, DataDict).

%%--------------------------------------------------------------------
%% Function: collect(Scale, DataList) -> ResultList
%% Description: Collects data with aggregation by Scale.
%%
%% DataList = [{DateTime, {SecurityName, Price, Amount}}]
%% Scale = minute | hour | day | week | month | year
%% ResultList = [{StartDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount}]
%%--------------------------------------------------------------------
collect(_, []) -> [];
collect(Scale, List) ->
	
	SortF = fun({Date1, _A}, {Date2, _B}) -> Date1 < Date2 end,

	[{Ctime, {_, Price, Amount}} | Rest] = lists:sort(SortF, List),

	{StartDateTime, Diff} = date_manipulation:scale_date(Ctime, Scale),

	CollectedData = collect_(Diff, {StartDateTime, Price, Price, Price, Price, Amount}, [], Rest),
	
	lists:reverse(CollectedData).

%%--------------------------------------------------------------------
%% Function: test(Scale) -> ResultList
%% Description: Test function wich uses test data and test filter (except Scale)
%% for generating test report.
%%
%% Scale = minute | hour | day | week | month | year
%% ResultList = [{StartDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount}]
%%--------------------------------------------------------------------
test(Scale) ->
	DataDict = test_data:test_data_dict(),
	Filter = {test_data:test_security_name(), test_data:test_datetime_period(), Scale},
	fetch_data(DataDict, Filter).

%%--------------------------------------------------------------------
%% Function: test_echo() -> {ResultFlag, ResultList}
%% Description: Test function wich uses test data and test filter
%% for generating test report specified in test_echo.pdf.
%% ResultFlag indicates whether the collected result list
%% is equal to test_data:test_result_data_list(). It must be true.
%%
%% ResultFlag = true | false
%% ResultList = [{StartDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount}]
%%--------------------------------------------------------------------
test_echo() ->
	ResultList = fetch_data(test_data:test_data_dict(), test_data:test_filter()),
	{ResultList == test_data:test_result_data_list(), ResultList}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: collect_(DateTimeDiff, Res, ResultList, RestDataList) -> ResultList
%% Description: Agregates data from RestDataList by periods.
%%
%% Periods are calculated as CurStartDateTime + DateTimeDiff.
%% Periods with no data in RestDataList are ignored.
%% OpenPrice seted in the period agregation begin.
%% ClosePrice, MaxPrice, MinPrice and TotalAmount collected in Res at the every period iteration.
%% In the last period iteration Res saves as new ResultList element.
%%
%% DateTimeDiff = {DiffScale, DiffValue}
%% DiffScale = seconds | months | years
%% DiffValue = Int
%% ResultList = [{StartDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount}]
%%--------------------------------------------------------------------
collect_ (_, Res, Results, []) -> [Res | Results];

collect_ (Diff, {CurStartDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount}, Results, [{Ctime, {_, Price, Amount}} | Rest]) ->
	NextStartTime = shift_start_time(Diff, CurStartDateTime),
	NextNextStartTime = shift_start_time(Diff, NextStartTime),
	Res = {CurStartDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount},
	if
		Ctime < NextStartTime ->
			LastResult = {CurStartDateTime, OpenPrice, Price, min(MinPrice, Price), max(MaxPrice, Price), TotalAmount + Amount},
			collect_ (Diff, LastResult, Results, Rest);

		(Ctime >= NextStartTime) and (Ctime < NextNextStartTime) ->
			collect_ (Diff, {NextStartTime, Price, Price, Price, Price, Amount}, [Res | Results], Rest);

		true ->
			{StTime, _} = get_next_start_time(Diff, Ctime, {NextStartTime, NextNextStartTime}),
			collect_ (Diff, {StTime, Price, Price, Price, Price, Amount}, [Res | Results], Rest)
	end.

%%--------------------------------------------------------------------
%% Function: shift_start_time(DateTimeDiff, DateTime) -> DateTime
%% Description: Shifts DateTime by DateTimeDiff.
%%
%% DateTimeDiff = {DiffScale, DiffValue}
%% DiffScale = seconds | months | years
%% DiffValue = Int
%%--------------------------------------------------------------------
shift_start_time(Diff, DateTime) -> date_manipulation:add_to_datetime(Diff, DateTime).

%%--------------------------------------------------------------------
%% Function: get_next_start_time(DateTimeDiff, CurrentDateTime, {DateTime1, DateTime2}) -> {DateTime1, DateTime2}
%% Description: Searches the next period, which contains CurrentDateTime.
%% Returns period {DateTime1 =< CurrentDateTime < DateTime2}.
%%
%% DateTimeDiff = {DiffScale, DiffValue}
%% DiffScale = seconds | months | years
%% DiffValue = Int
%%--------------------------------------------------------------------
get_next_start_time(Diff, Ctime, {Time1, Time2}) ->
	if
		(Ctime < Time2) and (Ctime >= Time1) -> {Time1, Time2};
		true -> get_next_start_time(Diff, Ctime, {Time2, date_manipulation:add_to_datetime(Diff, Time2)})
	end.

