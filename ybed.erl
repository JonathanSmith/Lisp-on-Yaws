-module(ybed).
-compile(export_all).

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "embedded",
    GConfList = [{id, Id}],
    Docroot = "/home/jon/common-yaws/www",
    SConfList = [{port, 8080},
                 {servername, "foobar"},
                 {listen, {0,0,0,0}},
                 {docroot, Docroot}],
    {ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf(Docroot, SConfList, GConfList, Id),
   [ supervisor:start_child(ybed_sup, Ch)  || Ch <- ChildSpecs ],
    yaws_api:setconf(GC, SCList),
    ConfControl = spawn(?MODULE, conf_loop, [Docroot, SConfList, GConfList, Id]),
    case register(confcontrol,ConfControl) of
	badarg ->
	    unregister(confcontrol),
	    register(confcontrol,ConfControl);
	true ->
	    ok		
    end,	
    {ok, self()}.

conf_loop(Docroot, SConfList, GConfList, Id) ->
    receive
	{sconf, SConfReplace} ->
	    NewSC = lists:keymerge(1,lists:keysort(1,SConfReplace),lists:keysort(1,SConfList)),
	    {ok, SCList, GC, _ChildSpecs} =
		yaws_api:embedded_start_conf(Docroot, NewSC, GConfList, Id),
	    yaws_api:setconf(GC, SCList),
	    conf_loop(Docroot, NewSC, GConfList, Id);

	{gconf, GConfReplace} ->
	    NewGC = lists:keymerge(1,lists:keysort(1,GConfReplace),lists:keysort(1,GConfList)),
	    {ok, SCList, GC, _ChildSpecs} =
		yaws_api:embedded_start_conf(Docroot, SConfList, NewGC, Id),
	    yaws_api:setconf(GC,SCList),
	    conf_loop(Docroot, SConfList, NewGC, Id);

	{update_appmod, AppMod} ->
	    io:format("updating appmod, ~w~n",[AppMod]),
	    case lists:keyfind(appmods,1,SConfList) of
		{appmods, Current_Appmods} ->
		    ok;
		false ->
		    Current_Appmods = []
	    end,
	    case AppMod of
		{_, AppModId} -> AppModId;
		{_, AppModId, _} -> AppModId
	    end,
	    
	    case lists:keyfind(AppModId,2,Current_Appmods) of
		false ->
		    New_Appmod_Tuple = {appmods, [AppMod | Current_Appmods]};
		_Tuple ->
		    New_Appmod_Tuple = {appmods, lists:keyreplace(AppModId,2,Current_Appmods,AppMod)}
	    end,
	    io:format("~w~n",[AppMod]),
	    io:format("~w~n",[New_Appmod_Tuple]),
	    case lists:keyfind(appmods,1,SConfList) of
		false ->
		    NewSC = [New_Appmod_Tuple | SConfList];
		_Tuple2 ->
		    NewSC = lists:keyreplace(appmods, 1, SConfList, New_Appmod_Tuple)
	    end,
	    io:format("~w~n",[NewSC]),
	    {ok, SCList, GC, _ChildSpecs} =
		yaws_api:embedded_start_conf(Docroot, NewSC, GConfList, Id),
	    io:format("~w~n",[SCList]),
	    yaws_api:setconf(GC, SCList),
	    conf_loop(Docroot, NewSC, GConfList, Id);

	{delete_appmod, AppModId} ->
	    case lists:keyfind(appmods,1,SConfList) of
	    {appmods, Current_Appmods} ->
		    case lists:keyfind(AppModId,2,Current_Appmods) of
			false ->
			    conf_loop(Docroot, SConfList, GConfList, Id);
			_Tuple ->
			    New_Appmod_Tuple = {appmods,lists:keydelete(AppModId,2,Current_Appmods)},
			    NewSC = lists:keyreplace(appmods, 1, SConfList, New_Appmod_Tuple),
			    {ok, SCList, GC, _ChildSpecs} =
				yaws_api:embedded_start_conf(Docroot, NewSC, GConfList, Id),
			    yaws_api:setconf(GC, SCList),
			    conf_loop(Docroot, NewSC, GConfList, Id)
		    end;
	    false ->
		    conf_loop(Docroot, SConfList, GConfList, Id)
	    end;
	{ping, PID} ->
	    PID ! {pong,self()}
    end.
