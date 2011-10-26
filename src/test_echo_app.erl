%%%-------------------------------------------------------------------
%%% File    : test_echo_app.erl
%%% Author  : Granin Alexandr <graninas@gmail.com>
%%% Description : test_echo application callback module.
%%%-------------------------------------------------------------------

-module(test_echo_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
    {ok, _Pid} = test_echo_sup:start_link().

stop(_State) ->
    ok.
