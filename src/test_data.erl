%%%-------------------------------------------------------------------
%%% File    : test_data.erl
%%% Author  : Granin Alexandr <graninas@gmail.com>
%%% Description : Functions with test data.
%%%-------------------------------------------------------------------

-module(test_data).

-export([test_data_assoc_list/0, test_data_dict/0]).
-export([test_datetime_begin/0, test_datetime_end/0, test_datetime_period/0,
		test_scale/0, test_security_name/0, test_filter/0, test_result_data_list/0]).

%%--------------------------------------------------------------------
%% Functions return test filter specified in test_echo.pdf.
%%--------------------------------------------------------------------
test_datetime_begin() -> {{2012, 4, 11}, {0, 0, 0}}.
test_datetime_end()   -> {{2012, 4, 12}, {0, 0, 0}}.
test_datetime_period() -> {test_datetime_begin(), test_datetime_end()}.
test_scale() -> hour.
test_security_name() -> 'ECHO'.
test_filter() -> {test_security_name(), test_datetime_period(), test_scale()}.

%%--------------------------------------------------------------------
%% Function: test_result_data_list() -> ResultList
%% Description: Returns test result list specified in test_echo.pdf.
%%
%% ResultList = [{StartDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount}]
%%--------------------------------------------------------------------
test_result_data_list() ->
	[
	{{{2012, 4,  11}, {10, 0, 0}}, 1.21, 1.22,  1.21, 1.22, 1600},
	{{{2012, 4,  11}, {11, 0, 0}}, 1.24, 1.26,  1.23, 1.26, 3500},
	{{{2012, 4,  11}, {12, 0, 0}}, 1.25, 1.25,  1.25, 1.25, 300 },
	{{{2012, 4,  11}, {13, 0, 0}}, 1.22, 1.23,  1.22, 1.23, 2200},
	{{{2012, 4,  11}, {14, 0, 0}}, 1.24, 1.22,  1.22, 1.25, 5200},
	{{{2012, 4,  11}, {15, 0, 0}}, 1.22, 1.22,  1.22, 1.22, 200 },
	{{{2012, 4,  11}, {16, 0, 0}}, 1.24, 1.24,  1.24, 1.24, 400 }].

%%--------------------------------------------------------------------
%% Function: test_data_dict() -> DataDict
%% Description: Returns dict with test data.
%%
%% DataDict = dict:from_list(DataList)
%% DataList = [{DateTime, {SecurityName, Price, Amount}}]
%%--------------------------------------------------------------------
test_data_dict() ->
	dict:from_list(test_data_assoc_list()).

%%--------------------------------------------------------------------
%% Function: test_data_assoc_list() -> DataList
%% Description: Returns list with test data.
%% List contains 15 records for 'ECHO' at {2012, 4, 11}, specified in test_echo.pdf;
%% and also it contains some records for 'ECHO' at other DateTime;
%% and also it contains some records for 'TEST' security.
%%
%% DataList = [{DateTime, {SecurityName, Price, Amount}}]
%%--------------------------------------------------------------------
test_data_assoc_list() ->
	[
	{{{2012, 4, 11}, {9,  5,  0}}, {'TEST', 1.10, 600 }},
	{{{2012, 4, 11}, {10, 10, 0}}, {'TEST', 1.15, 952 }},
	{{{2012, 4, 11}, {10, 31, 0}}, {'ECHO', 1.21, 1200}},
	{{{2012, 4, 11}, {10, 45, 0}}, {'TEST', 1.17, 1200}},
	{{{2012, 4, 11}, {10, 52, 0}}, {'ECHO', 1.22, 400 }},
	{{{2012, 4, 11}, {11, 16, 0}}, {'ECHO', 1.24, 1300}},
	{{{2012, 4, 11}, {11, 21, 0}}, {'ECHO', 1.23, 1000}},
	{{{2012, 4, 11}, {11, 48, 0}}, {'ECHO', 1.26, 1200}},
	{{{2012, 4, 11}, {12, 20, 0}}, {'ECHO', 1.25, 300 }},
	{{{2012, 4, 11}, {12, 32, 0}}, {'TEST', 1.18, 100 }},
	{{{2012, 4, 11}, {13, 05, 0}}, {'ECHO', 1.22, 200 }},
	{{{2012, 4, 11}, {13, 44, 0}}, {'ECHO', 1.23, 2000}},
	{{{2012, 4, 11}, {14, 01, 0}}, {'ECHO', 1.24, 1400}},
	{{{2012, 4, 11}, {14, 13, 0}}, {'ECHO', 1.25, 500 }},
	{{{2012, 4, 11}, {14, 22, 0}}, {'ECHO', 1.24, 700 }},
	{{{2012, 4, 11}, {14, 37, 0}}, {'ECHO', 1.23, 1100}},
	{{{2012, 4, 11}, {14, 39, 0}}, {'ECHO', 1.22, 1500}},
	{{{2012, 4, 11}, {15, 04, 0}}, {'ECHO', 1.22, 200 }},
	{{{2012, 4, 11}, {16, 32, 0}}, {'ECHO', 1.24, 400 }},
	{{{2012, 4, 12}, {22, 10, 0}}, {'ECHO', 1.33, 544 }},
	{{{2012, 4, 12}, {23, 45, 0}}, {'ECHO', 1.35, 1500}},
	{{{2012, 4, 12}, {23, 49, 0}}, {'ECHO', 1.21, 1210}},
	{{{2012, 5,  1}, {5 , 14, 0}}, {'ECHO', 1.19, 778 }},
	{{{2012, 5,  1}, {7 , 22, 0}}, {'ECHO', 1.16, 512 }},
	{{{2012, 5,  1}, {8 , 0 , 0}}, {'ECHO', 1.28, 1445}},
	{{{2012, 5,  1}, {8 , 10, 0}}, {'ECHO', 1.29, 2144}}].



