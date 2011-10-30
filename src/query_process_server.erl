%%%-------------------------------------------------------------------
%%% File    : query_process_server.erl
%%% Author  : Granin Alexandr <graninas@gmail.com>
%%% Description : The POST/GET process server.
%%%-------------------------------------------------------------------

-module(query_process_server).
-behaviour(gen_server).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-export([datetime_extract/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% Server post/get processing API.
%%====================================================================

%%--------------------------------------------------------------------
%% Function: datetime_extract(Options) -> {DateTime, StringDateTime}
%% Description: Extracts posted or geted date/time values stored in A-variable.
%% Returns ordinary DateTime structure and structure with all date/time values as strings.
%% DateTime = datetime()
%% StringDateTime = {{StrYear, StrMonth, StrDay}, {StrHour, StrMinute, StrSecond}}
%% Options = {A, QueryMethod, {YearVar, MonthVar, DayVar, HourVar, MinuteVar, SecondVar}}
%% QueryMethod = post | 'get'
%% Vars = string()
%%--------------------------------------------------------------------
datetime_extract(Options) ->
	gen_server:call(?SERVER, {datetime_extract, Options}).

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
	{ok, {ehtml, {p, [], "Query process server ok."}}}.

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

% Called for datetime extraction.
handle_call({datetime_extract, Options}, _From, State) ->
	{reply, extract_data(datetime, Options), State};

% Called in undefined cases.
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

extract_data(datetime, {A, QueryMethod, {YearVar, MonthVar, DayVar, HourVar, MinuteVar, SecondVar}}) ->

	GetVarF = 
		case QueryMethod of
			post -> fun(Arg, Var) -> yaws_api:postvar(Arg, Var) end;
			_    -> fun(Arg, Var) -> yaws_api:getvar (Arg, Var) end
		end,

	{ok, SDay}    = GetVarF(A, DayVar),
	{ok, SMonth}  = GetVarF(A, MonthVar),
	{ok, SYear}   = GetVarF(A, YearVar),
	{ok, SHour}   = GetVarF(A, HourVar),
	{ok, SMinute} = GetVarF(A, MinuteVar),
	{ok, SSecond} = GetVarF(A, SecondVar),
	Y  = list_to_integer(SYear),
	M  = list_to_integer(SMonth),
	D  = list_to_integer(SDay),
	H  = list_to_integer(SHour),
	MM = list_to_integer(SMinute),
	S  = list_to_integer(SSecond),

	{{{Y,M,D}, {H,MM,S}}, {{SYear, SMonth, SDay}, {SHour, SMinute, SSecond}}}.
