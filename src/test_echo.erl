%%%-------------------------------------------------------------------
%%% File    : test_echo.erl
%%% Author  : Granin Alexandr <graninas@gmail.com>
%%% Description : Application file
%%%-------------------------------------------------------------------

-module(test_echo).
-compile(export_all).

%% Starting application.
start() ->
    {ok, spawn(?MODULE, run, [])}.


%% Starting YAWS embedded.
run() ->
    Id = "embedded",
    GconfList = [{id, Id}],

    Docroot = 
		case application:get_env(docroot) of
			{ok, DRV} -> DRV;
			_ -> "/home/gas/Erlang/TestEcho/priv/docroot"
		end,

	Host =
		case application:get_env(host) of
			{ok, HV} -> HV;
			_ -> {0,0,0,0}
		end,

	Port =
		case application:get_env(port) of
			{ok, PV} -> PV;
			_ -> 8080
		end,

	ServerName =
		case application:get_env(server_name) of
			{ok, SNV} -> SNV;
			_ -> "test_echo"
		end,

    SconfList = [{port, Port},
                 {servername, ServerName},
                 {listen, Host},
                 {docroot, Docroot}],

    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(test_echo_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.













