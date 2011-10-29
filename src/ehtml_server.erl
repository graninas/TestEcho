%%%-------------------------------------------------------------------
%%% File    : ehtml_server.erl
%%% Author  : Granin Alexandr <graninas@gmail.com>
%%% Description : The ehtml formatting server.
%%%-------------------------------------------------------------------

-module(ehtml_server).
-behaviour(gen_server).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-export([list_to_table/1, date_input_template/1, time_input_template/1]).

-import(mochinum, [digits/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% Server data manipulation API.
%%====================================================================

list_to_table(DataList) ->
	gen_server:call(?SERVER, {list_to_table, DataList}).

date_input_template(PredefVals) ->
	gen_server:call(?SERVER, {date_input_template, PredefVals}).

time_input_template(PredefVals) ->
	gen_server:call(?SERVER, {time_input_template, PredefVals}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server, returns basic ehtml.
%%--------------------------------------------------------------------
init(_Args) ->
	{ok, {ehtml, {p, [], "Ehtml server ok."}}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) ->
%%										{reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({_, []}, _From, State) ->
	Res = {p, [], "No data."},
	{reply, Res, State};

handle_call({list_to_table, DataList}, _From, State) ->
	Res = {table, [], make_table_rows(DataList)},
	{reply, Res, State};

handle_call({date_input_template, PredefVals}, _From, State) ->
	Res = make_date_input_template(PredefVals),
	{reply, Res, State};

handle_call({time_input_template, PredefVals}, _From, State) ->
	Res = make_time_input_template(PredefVals),
	{reply, Res, State};

%% Called for unregistered cases.
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

%% Casted for unregistered cases.
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions.
%%====================================================================

make_date_input_template({{DayInputName, DayDefVal},
						  {MonthInputName, _MonthDefVal},
						  {YearInputName, YearDefVal}}) ->
	[{input, [{name, DayInputName},  {type, text}, {maxlength, "2"}, {size, "2"}, {value, DayDefVal}]},
	 {select, [{name, MonthInputName}], [
			{option, [{value, "1"}], "January"},
			{option, [{value, "2"}], "Febuary"},
			{option, [{value, "3"}], "March"},
			{option, [{value, "4"}], "April"},
			{option, [{value, "5"}], "May"},
			{option, [{value, "6"}], "June"},
			{option, [{value, "7"}], "July"},
			{option, [{value, "8"}], "August"},
			{option, [{value, "9"}], "September"},
			{option, [{value, "10"}], "October"},
			{option, [{value, "11"}], "November"},
			{option, [{value, "12"}], "December"}
		]},
	 {input, [{name, YearInputName}, {type, text}, {value, YearDefVal}, {maxlength, "4"}, {size, "4"}]}].

make_time_input_template({{HourInputName, HourDefVal},
                          {MinuteInputName, MinuteDefVal},
                          {SecondInputName, SecondDefVal}}) ->
	[{input, [{name, HourInputName  }, {type, text}, {maxlength, "2"}, {size, "2"}, {value, HourDefVal   }]},
	 {input, [{name, MinuteInputName}, {type, text}, {maxlength, "2"}, {size, "2"}, {value, MinuteDefVal }]},
	 {input, [{name, SecondInputName}, {type, text}, {maxlength, "2"}, {size, "2"}, {value, SecondDefVal }]}].

make_table_rows([DataItem | []]) ->
	[make_table_row(DataItem)];

make_table_rows([DataItem | RestList]) ->
	[make_table_row(DataItem)] ++ make_table_rows(RestList).

%% For SecurityTransaction format
make_table_row({DateTime, SecData}) ->
	{SecurityName, Price, Amount} = SecData,

	{tr, [], [
		{td, [], datetime_to_string(DateTime)},
		{td, [], atom_to_list(SecurityName)},
		{td, [], md(Price)},
		{td, [], md(Amount)}
	]};

% ResultList = [{StartDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount}]
make_table_row({DateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount})
	when erlang:is_list(DateTime) ->
		{tr, [], [
			{td, [], DateTime},
			{td, [], OpenPrice},
			{td, [], ClosePrice},
			{td, [], MinPrice},
			{td, [], MaxPrice},
			{td, [], TotalAmount}
		]};

make_table_row({DateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount}) -> 
	{tr, [], [
		{td, [], datetime_to_string(DateTime)},
		{td, [], md(OpenPrice)},
		{td, [], md(ClosePrice)},
		{td, [], md(MinPrice)},
		{td, [], md(MaxPrice)},
		{td, [], md(TotalAmount)}
	]};
	
%% For test cases format
make_table_row({A, B, C, D}) ->
	{tr, [], [
		{td, [], A},
		{td, [], B},
		{td, [], C},
		{td, [], D}
	]}.


md(Num) -> mochinum:digits(Num).

datetime_to_string({{Year, Month, Day},{Hour, Minute, Second}}) ->
	integer_to_list(Day)
	++ "/" ++ integer_to_list(Month)
	++ "/" ++ integer_to_list(Year)
	++ " " ++ integer_to_list(Hour)
	++ ":" ++ integer_to_list(Minute)
	++ ":" ++ integer_to_list(Second).

	

