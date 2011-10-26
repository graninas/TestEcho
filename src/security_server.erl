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

-export([data/0, add_transaction/1, report/1]).

-import(data_collection, [fetch_data/2, filter_data/2, collect/2, test/1]).

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
	gen_server:cast(?SERVER, {add_transaction, Transaction}).

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

%% Called for unregistered cases.
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% Casted for adding new data.
handle_cast({add_transaction, Transaction}, State) ->
	{DateTime, Security, Price, Amount} = Transaction,
	{noreply, dict:store(DateTime, {Security, Price, Amount}, State)};

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



