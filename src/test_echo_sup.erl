-module(test_echo_sup).
-behaviour(supervisor).

-export([start_link/0, start/0, start_tracer/0]).

-export([init/1]).

start() -> start_link().

start_tracer() ->
	dbg:tracer(),
	dbg:p(all,c),
	dbg:tpl(security_server,x),
	dbg:tpl(test_echo_sup, x),
	dbg:p(new,m),
	dbg:p(new,p).

start_link() ->
    {ok, PID} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
	unlink(PID). %PID = console pid(). Unlinked for console-crash independing.

init([]) ->
    TestEcho = {test_echo, {test_echo, start, []}, permanent,2000,worker,[test_echo]},
	SecurityServer = {security_server, {security_server, start, []}, permanent,2000,worker,[security_server]},
	EhtmlServer = {ehtml_server, {ehtml_server, start, []}, permanent,2000,worker,[ehtml_server]},

    {ok,{{one_for_one,0,1}, [TestEcho, SecurityServer, EhtmlServer]}}.

