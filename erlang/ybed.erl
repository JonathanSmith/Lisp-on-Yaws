-module(ybed).
-compile(export_all).

start() ->
    {ok, spawn(?MODULE, run, [])}.

spawn_and_register(RegId, Module, FunId, FunArgs) ->
    Process = spawn(Module, FunId, FunArgs),
    case register(RegId,Process) of
	badarg ->
	    unregister(RegId),
	    register(RegId,Process),
	    ok;
	true ->
	    ok
    end.

run() ->
    Id = "embedded",
    GConfList = [{id, Id}],
    Docroot = filename:absname("") ++ "/www",
    Ebin = filename:absname("") ++ "/ebin/",
    SrcDir = filename:absname("") ++ "/src/",
    SConfList = [{port, 8080},
                 {servername, "foobar"},
                 {listen, {0,0,0,0}},
                 {docroot, Docroot},
		{partial_post_size, nolimit}],
    {ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf(Docroot, SConfList, GConfList, Id),
    [supervisor:start_child(ybed_sup, Ch)  || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    spawn_and_register(conf_control, ?MODULE, conf_loop, [Docroot, SConfList, GConfList, Id]),
    spawn_and_register(lfe_compiler, ?MODULE, lfe_comp_loop, [Ebin,SrcDir]),
    spawn_and_register(write_page, ?MODULE, write_static_page_loop, [Docroot ++ "/"]),
    {ok, self()}.

write_static_page_loop(Docroot) ->
    receive {Path,FileName,PageData} ->
	    FilePath = Docroot ++ Path ++ "/" ++ FileName,
	    case file:write_file(FilePath,PageData) of
		ok -> ok;
		{error,enoent} -> 
		    file:make_dir(Docroot ++ Path ++ "/"),
		    file:write_file(FilePath,PageData);
		{error,enotdir} ->
		    file:make_dir(Docroot ++ Path ++ "/"),
		    file:write_file(FilePath,PageData);
		ELSE ->
		    io:format("~w ~w ~n",[ELSE,FilePath])
			
	    end;
	    {FileName,PageData} ->
	    FilePath = Docroot ++ FileName,
	    file:write_file(FilePath,PageData)
    end,
    write_static_page_loop(Docroot).

lfe_comp_loop(Ebin,SrcDir) ->
    receive {file,ModuleName, FileData} ->
	 spawn (fun () ->
		    io:format("write: ~p ~n",[ModuleName]),
		    FilePath = SrcDir ++ ModuleName,
		    ok = file:write_file(FilePath ++ ".lfe", FileData),
		    io:format("comp: ~p ~n",[ModuleName]),
		    lfe_comp:file(FilePath,[{outdir,Ebin},report]),
		    ModuleAtom = list_to_atom(ModuleName),
		    io:format("load: ~p ~n",[ModuleName]),
		    case code:is_loaded(ModuleAtom) of
			{file, _Loaded} ->
			    code:purge(ModuleAtom),
			    code:load_file(ModuleAtom);
			false ->
			    code:load_file(ModuleAtom)
		    end,
		    io:format("done: ~p ~n",[ModuleName])
	    end),
	    lfe_comp_loop(Ebin,SrcDir)
    end.

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
		{_, AppModId, _} -> AppModId;
		AppModId -> AppModId
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
	    PID ! {pong,self()},
	    conf_loop(Docroot, SConfList, GConfList, Id);
	ANYTHING ->
	    io:format("wrong command: ~p ~n",[ANYTHING]),
	    conf_loop(Docroot, SConfList, GConfList, Id)
    end.
