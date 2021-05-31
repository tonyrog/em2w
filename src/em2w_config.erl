%%
%% Read an xml spec and transform it into a simple term format
%% saved as .config
%%
-module(em2w_config).

-export([compile/1, compile/0]).
-export([load/1, load/0]).
-export([modified/2, modified_file/2]).

-compile(export_all).


-import(lists, [map/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/file.hrl").

compile() -> compile("config").
compile(File) ->
    Source = File ++ ".xml",
    case scan(Source) of
	{error,Reason} ->
	    {error, Reason};
	Conf ->
	    Target = File,
	    file:rename(Target, Target ++ ".bak"),
	    ok = file:write_file(Target, term_to_binary(Conf)),
	    {ok,Conf}
    end.
    
scan(File) ->
    Rules = ets:new(rule, [set]),
    case catch xmerl_scan:file(File,[{rules, Rules}]) of
	{'EXIT', Reason} ->
	    ets:delete(Rules),
	    {error, {'EXIT',Reason}};
	{error, Reason} ->
	    ets:delete(Rules),
	    {error, Reason};
	{Element, Rest} ->
	    Res = rebuild(Element, Rules),
	    ets:delete(Rules),	    
	    Res
    end.

%% Read a config file
read(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    case catch binary_to_term(Bin) of
		{'EXIT', Reason} ->
		    {error, {'EXIT', Reason}};
		Conf ->
		    case Conf of
			{'Em2w',[{version,"1.0"}], _} ->
			    {ok,Conf};
			_ ->
			    {error, bad_config}
		    end
	    end;
	Error ->
	    Error
    end.

%% Safe read
safe_read(File) ->
    case read(File) of
	{error,Reason} ->
	    io:format("Error: ~p\n", [Reason]),
	    io:format("Trying backup\n"),
	    case read(File++".bak") of
		{error, Reason} ->
		    io:format("Read of backup failed: ~p\n",[Reason]),
		    {error, Reason};
		{ok,Conf} ->
		    {ok,Conf}
	    end;
	{ok,Conf} ->
	    {ok,Conf}
    end.

	    
%% Load (compile-load-if-modified)
load() -> load("config").
    
load(File) ->
    case file:read_file_info(File++".xml") of
	{ok, SrcInfo} ->
	    case file:read_file_info(File) of
		{ok,DstInfo} ->
		    case modified(SrcInfo, DstInfo) of
			true ->
			    io:format("Load: Modified\n"),
			    case compile(File) of
				{ok,Conf} ->
				    {ok,Conf};
				Error ->
				    safe_read(File)
			    end;
			false ->
			    io:format("Load: Not modified\n"),
			    safe_read(File)
		    end;
		{error,Reason} ->
		    io:format("Load: Compile: because ~p\\n",[Reason]),
		    case compile(File) of
			{ok,Conf} ->
			    {ok,Conf};
			Error ->
			    safe_read(File)
		    end
	    end;
	{error,Reason} ->
	    {error, no_config}
    end.

modified_file(SrcFile, DstFile) ->
    {ok,Src} = file:read_file_info(SrcFile),
    {ok,Dst} = file:read_file_info(DstFile),
    modified(Src,Dst).
    

modified(Src, Dst) ->
    S = calendar:datetime_to_gregorian_seconds(Src#file_info.mtime),
    D = calendar:datetime_to_gregorian_seconds(Dst#file_info.mtime),
    S > D.

%%
%% Path - collect users and content types
%%
%%  site: collect global+site 
%%  page: collect global+site+page 
%%  item: collect global+site+page+item
path([S], {'Em2w',[{version,"1.0"}|_],Elems}) ->
    path_collect([{'Site',S}], Elems, []);
path([S,P], {'Em2w',[{version,"1.0"}|_],Elems}) ->
    path_collect([{'Site',S},{'Page',P}], Elems, []);
path([S,P,I], {'Em2w',[{version,"1.0"}|_],Elems}) ->
    path_collect([{'Site',S},{'Page',P},{'Item',I}], Elems, []).


path_collect([{Type,Id}|Ts], [{Type,Attrs,Elems}|Es], Acc) ->
    case lists:keysearch(id, 1, Attrs) of
	{value, {_, Id}} ->
	    path_collect(Ts, Elems, Acc);
	_ ->
	    path_collect([{Type,Id}|Ts], Es, Acc)
    end;
path_collect(Ts, [{'User',Attrs,Elems}|Es], Acc) ->
    path_collect(Ts, Es, [{'User',Attrs,Elems}|Acc]);
path_collect(Ts, [{'Content',Attrs,Elems}|Es], Acc) ->
    path_collect(Ts, Es, [{'Content',Attrs,Elems}|Acc]);
path_collect(Ts, [_|Es], Acc) ->
    path_collect(Ts, Es, Acc);
path_collect(_, _, Acc) -> Acc.

%%
%% Check user access and content types
%% return {ok, {Id, User, Content}}
%%
user_path(Path, UId, Conf) ->
    user_collect(path(Path, Conf), UId, false, [], []).

user_collect([{'User',Attrs,Elems}|Es], UId, Found, UAcc, CAcc) ->
    case lists:keysearch(id, 1, Attrs) of
	{value, {_, UId}} ->
	    %% FIXME: do a expression match!
	    user_collect(Es, UId, true, Attrs++UAcc, Elems++CAcc);
	_ ->
	    user_collect(Es, UId, Found, UAcc, CAcc)
    end;
user_collect([C={'Content',Attrs,Elems}|Es], UId, Found, UAcc, CAcc) ->
    user_collect(Es, UId, Found, UAcc, [C|CAcc]);
user_collect([], UId, true, UAcc, CAcc) ->
    {value, {UId, UAcc, CAcc}};
user_collect([], UId, false, UAcc, CAcc) ->
    false.
    
    




rebuild(E,R) when is_record(E, xmlElement) ->
    Content = rebuild_list(E#xmlElement.content,R),
    Attr0   = map(fun reattr/1, E#xmlElement.attributes),
    Attr1   = defattr(E#xmlElement.name, Attr0, R),
    {E#xmlElement.name, Attr1, Content};
rebuild(E,R) when is_record(E, xmlText) ->
    {error, bad_content}.


rebuild_list([E|Es],R) when is_record(E, xmlElement) ->
    Content = rebuild_list(E#xmlElement.content,R),
    Attr0   = map(fun reattr/1, E#xmlElement.attributes),
    Attr1   = defattr(E#xmlElement.name, Attr0, R),
    [{E#xmlElement.name, Attr1, Content}|rebuild_list(Es,R)];
rebuild_list([_|Es],R) ->    
    rebuild_list(Es,R);
rebuild_list([],R) ->
    [].

defattr(ElemName, Attr0, R) ->
    [{_, Rule}] = ets:lookup(R, {elem_def, ElemName}),
    DefList = map(fun({A,_,V}) -> {A,V} end, Rule#xmlElement.attributes),
    io:format("deflist: ~p\n", [DefList]),
    defadd(DefList, ElemName, Attr0).


defadd([{A,V}|As], ElemName, Attr0) ->
    case lists:keysearch(A, 1, Attr0) of
	false ->
	    case V of
		'#REQUIRED' ->
		    io:format("missing required value for ~s in element ~s\n", 
			      [A, ElemName]),
		    defadd(As, ElemName, Attr0);
		'#IMPLIED' ->
		    defadd(As, ElemName, Attr0);
		_ when is_list(V) ->
		    defadd(As, ElemName, [{A,V}|Attr0]);
		_ ->
		    defadd(As, ElemName, Attr0)
	    end;
	_ ->
	    defadd(As, ElemName, Attr0)
    end;	    
defadd([], ElemName, Attr0) ->	    
    Attr0.

    
reattr(Attr) ->
    {Attr#xmlAttribute.name, Attr#xmlAttribute.value}.

    
    
    
    

