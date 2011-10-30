{application, test_echo, [
	{vsn, "0.9.0"},
	{modules, [test_echo, test_echo_app, test_echo_sup,
				security_server, query_process_server, ehtml_server,
				data_collection, date_manipulation,
				test_data,
				mochinum]},
	{description,  "Test program for Security Transactions."},
	{mod, {test_echo_app, []}},
	{env, [{host, {0,0,0,0}},
		   {port, 8081},		   
		   {server_name, "test_echo"},
		   {approot, "../priv/docroot"},

		   {data_store_server, security_server},
		   {data_process_server, security_server},
		   {ehtml_format_server, ehtml_server},
		   {query_process_server, query_process_server}]},
	{applications, [kernel, stdlib]}
]}.
