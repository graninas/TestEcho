%%%-------------------------------------------------------------------
%%% File    : security_server.erl
%%% Author  : Granin Alexandr <graninas@gmail.com>
%%% Description : The security transacions server.
%%% Template taken from http://spawnlink.com/otp-intro-1-gen_server-skeleton/
%%% Template author: Mitchell Hashimoto
%%%-------------------------------------------------------------------

-module(security_server).
-behaviour(gen_server).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-export([data/0, set_data/1, clear_data/0]).
-export([add_transaction/1, report/1]).
-export([load_test_data/0, merge_test_data/0]).

-import(data_collection, [fetch_data/2, filter_data/2, collect/2, test/0]).
-import(test_data, [test_data_dict/0]).

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

%%--------------------------------------------------------------------
%% Function: add_transaction(Transaction) -> {noreply, State}
%% Description: Adds new transaction into the data Dict.
%%
%% Transaction = {DateTime, {SecurityName, Price, Amount}}
%%--------------------------------------------------------------------
add_transaction(Transaction) ->
	gen_server:call(?SERVER, {add_transaction, Transaction}).

%%--------------------------------------------------------------------
%% Function: report(Filter) -> {reply, ResultList, State} |
%%                             {reply, ok, State}
%% Description: Extracts data from data Dict according to Filter and report format.
%%
%% Filter = {SecurityName, {DateTimeBegin, DateTimeEnd}, Scale}
%% ResultList = [{StartDateTime, OpenPrice, ClosePrice, MinPrice, MaxPrice, TotalAmount}]
%% Scale = minute | hour | day | week | month | year
%%--------------------------------------------------------------------
report(Filter) ->
	gen_server:call(?SERVER, {report_data, Filter}).

%%--------------------------------------------------------------------
%% Function: data() -> {reply, ResultDict, State}
%% Description: Returns data Dict.
%%--------------------------------------------------------------------
data() ->
	gen_server:call(?SERVER, get_data).

load_test_data() ->
	gen_server:call(?SERVER, load_test_data).

merge_test_data() ->
	gen_server:call(?SERVER, merge_test_data).

set_data(NewDict) ->
	gen_server:cast(?SERVER, {set_data, NewDict}).

clear_data() ->
	gen_server:cast(?SERVER, clear_data).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server, returns empty Dict.
%%--------------------------------------------------------------------
init(_Args) ->
	{ok, dict:new()}.

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
%% Called for data extracting.
handle_call({report_data, Filter}, _, State) ->
	ResultList = data_collection:fetch_data(State, Filter),
	{reply, ResultList, State};

%% Called for data returning.
handle_call(get_data, _, State) ->
	{reply, State, State};

%% Called for adding transaction.
handle_call({add_transaction, Transaction}, _, State) ->
	{DateTime, SecData} = Transaction,
	NewState = dict:store(DateTime, SecData, State),
	{reply, NewState, NewState};

handle_call(load_test_data, _, OldDict) ->
	TestDict = test_data:test_data_dict(),
	{reply, {OldDict, TestDict}, TestDict};

handle_call(merge_test_data, _, OldDict) ->
	TestDict = test_data:test_data_dict(),
	MergeF = fun(_, Val1, _) -> Val1 end,
	NewDict = dict:merge(MergeF, TestDict, OldDict),
	{reply, NewDict, NewDict};

%% Called for unregistered cases.
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({set_data, NewDict}, _) ->
	{noreply, NewDict};

handle_cast(clear_data, _) ->
	{noreply, dict:new()};

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



