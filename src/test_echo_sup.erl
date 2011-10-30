-module(test_echo_sup).
-behaviour(supervisor).

-export([start_link/0, start/0, start_tracer/0]).

-export([init/1]).

% Starting supervisor.
start() -> start_link().

% Starting debugging (if needed).
start_tracer() ->
	dbg:tracer(),
	dbg:p(all,c),
	dbg:tpl(security_server,x),
	dbg:tpl(test_echo_sup, x),
	dbg:p(new,m),
	dbg:p(new,p).

start_link() ->
    {ok, _PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, []).


% Starting servers, application.
init([]) ->
    TestEcho = {test_echo, {test_echo, start, []}, permanent,2000,worker,[test_echo]},
	SecurityServer = {security_server, {security_server, start, []}, permanent,2000,worker,[security_server]},
	EhtmlServer = {ehtml_server, {ehtml_server, start, []}, permanent,2000,worker,[ehtml_server]},
	QueryProcessServer = {query_process_server, {query_process_server, start, []}, permanent,2000,worker,[query_process_server]},

    {ok,{{one_for_one,0,1}, [TestEcho, SecurityServer, EhtmlServer, QueryProcessServer]}}.

