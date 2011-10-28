{application, test_echo, [
	{vsn, "0.1.0"},
	{modules, [test_echo, test_echo_app, test_echo_sup, test_echo_handler,
				security_server, data_collection,
				date_manipulation, test_data, ehtml_server]},
	{description,  "Test program for Security Transactions."},
	{mod, {test_echo_app, []}},
	{env, [{data_store_server, security_server},
		   {data_process_server, security_server},
		   {ehtml_format_server, ehtml_server}]},
	{applications, [kernel, stdlib]}
]}.
