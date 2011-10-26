-module(test_echo_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    TestEcho = {test_echo, {test_echo, start, []}, permanent,2000,worker,[test_echo]},
	SecurityServer = {security_server, {security_server, start, []}, permanent,2000,worker,[security_server]},

    {ok,{{one_for_all,0,1}, [TestEcho, SecurityServer]}}.

