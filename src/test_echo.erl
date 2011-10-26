-module(test_echo).
-compile(export_all).

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "embedded",
    GconfList = [{id, Id}],
    Docroot = "/home/gas/Erlang/TestEcho/priv/docroot",
    SconfList = [{port, 8080},
                 {servername, "test_echo"},
                 {listen, {0,0,0,0}},
                 {docroot, Docroot}],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(test_echo_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.













