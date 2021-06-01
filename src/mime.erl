%%% File    : mime.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : Mime scanner
%%% Created :  9 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(mime).

-compile(export_all).

-import(lists, [reverse/1, map/2, foreach/2, filter/2]).
-import(string,[tokens/2]).

-define(debug, true).

-ifdef(debug).
-define(dbg(Fmt,Args), io:format(Fmt, Args)).
-else.
-define(dbg(Fmt,Args), ok).
-endif.

-include("../include/mime.hrl").

%%
%% Map mime file
%%
map_mime(File,Bound) ->
    case file:open(File, [raw,read,binary]) of
	{ok, Fd} ->
	    Desc = {Fd, <<>>},
	    Res = scan_content(Desc,Bound),
	    file:close(Fd),
	    case Res of
		{ok,_,Content} ->
		    {ok,Content};
		Error ->
		    Error
	    end;
	Error -> Error
    end.

map_mime(File) ->
    map_mime(File, <<>>).

%% Display mime content
display(File, Bound) ->
    case map_mime(File, Bound) of
	{ok,Content} ->
	    Content1 = name_content(Content),
	    CidMap = cid_map(Content1, "./"),
	    io:format("CID MAP=~p\n", [CidMap]),
	    display_content(Content1);
	Error ->
	    Error
    end.

%%
%% Unpack an mime formatted file
%%
unpack(File) ->
    unpack(File, ".", "/e2w").

unpack(File, OutDir, WebDir) ->
    case file:open(File, [raw,read,binary]) of
	{ok, Fd} ->
	    Desc = {Fd, <<>>},
	    Res = scan_content(Desc,<<>>),
	    case Res of
		{ok,_,Content} ->
		    Content1 = name_content(Content),
		    CidMap = cid_map(Content1, WebDir),
		    save_content(OutDir, CidMap, Fd, Content1, false),
		    file:close(Fd),
		    {ok,Content1};
		Error ->
		    file:close(Fd),
		    Error
	    end;
	Error -> 
	    Error
    end.



%% Scan mbox content
map_mbox(File) ->
    map_mime(File, <<"From ">>).

display_mbox(File) ->
    display(File, <<"From ">>).

%%
%% Unpack an mbox formatted file
%%
unpack_mbox(File) ->
    unpack_mbox(File, ".", "/e2w").

unpack_mbox(File, OutDir, WebDir) ->
    case file:open(File, [raw,read,binary]) of
	{ok, Fd} ->
	    Desc = {Fd, <<>>},
	    Res = scan_content(Desc, <<"From ">>),
	    case Res of
		{ok,_,Content} ->
		    Content1 = name_content(Content),
		    CidMap = cid_map(Content1, WebDir),
		    save_content(OutDir, CidMap, Fd, Content1, true),
		    file:close(Fd),
		    {ok,Content1};
		Error ->
		    file:close(Fd),
		    Error
	    end;
	Error -> 
	    Error
    end.


%%
%% Unpack a mail message to the em2w format
%% i.e top level data should be entered into main.inc
%%
%% top level cases:
%%   1 - multipart/alternative
%%
%%      Select:
%%          a. html part (if available) and format html doc as
%%             html flow (i.e only body content)
%%          b. 
%% 
%%   2 - multipart/releated
%%        Select:
%%         a.  text/html part (if available) and format html doc as
%%             html flow (i.e only body content)
%%         b.  text/plain
%%         c. multipart/alternative and recurse
%%
%%   3 - multipart/mixed - append for each to main.inc
%%
%%      a) htmlified text/plain
%%      b) body part of any html content
%%      c) <img src=...> for image/<type>
%%      d) <a href=...> for other types
%%
%%
%%   4 - multipart/signed - recurse over the content (first part)
%%
%%
%%   5 - non multipart -  add to main.inc:
%% 
%%      a) htmlified text/plain
%%      b) body part of any html content
%%      c) <img src=...> for image/<type>
%%      d) <a href=...> for other types
%%
unpack_em2w(File) ->
    unpack_em2w(File, ".", "/e2w").

unpack_em2w(File, OutDir, WebDir) ->
    case file:open(File, [raw,read,binary]) of
	{ok, Fd} ->
	    Desc = {Fd, <<>>},
	    Res = scan_content(Desc, <<"From ">>),
	    case Res of
		{ok,_,Content} ->
		    Content1 = name_content(Content),
		    CidMap = cid_map(Content1, WebDir),
		    save_content(OutDir, CidMap, Fd, Content1, true),
		    Main = main_content(CidMap, Fd, WebDir, Content1, true),
		    file:write_file(filename:join(OutDir, "main.inc"),
				    list_to_binary([Main])),
		    file:close(Fd),
		    {ok,Content1};
		Error ->
		    file:close(Fd),
		    Error
	    end;
	Error -> 
	    Error
    end.

	    
%%
%% scan mime content
%%

scan_content(Desc,Bound) ->
    ?dbg("scan_content: ~s\n", [binary_to_list(Bound)]),
    {ok,HStart} = position(Desc),
    case scan_headers(Desc) of
	{ok,Desc1,Hs} ->
	    {ok,Start} = position(Desc1),
	    case lists:keysearch('CONTENT-TYPE', 1, Hs) of
		{value, {_, [Type|Params]}} ->
		    MimeType = string:to_lower(Type),
		    case lists:keysearch("boundary", 1, Params) of
			{value, {_, Boundary}} ->
			    UBound = unquote(Boundary),
			    Bound1 = list_to_binary("--"++UBound),
			    case scan_content_parts(Desc1, Bound1) of
				{ok,Desc2,Parts} ->
				    C = #mime { type = MimeType,
						headers = Hs, 
						hstart = HStart,
						start = Start,
						parts = Parts },
				    scan_part(Desc2,Bound,C);
				Error ->
				    Error
			    end;
			false ->
			    if MimeType == "message/rfc822" ->
				    case scan_content(Desc1, Bound) of
					{ok,Desc2,Content} ->
					    {ok,Stop} = position(Desc2),
					    C = #mime { type = MimeType,
							headers = Hs,
							hstart = HStart,
							start = Start,
							stop = Stop,
							parts = [Content]},
					    {ok,Desc2, C};
					Error -> Error
				    end;
			       true ->
				    C = #mime { type = MimeType,
						headers = Hs, 
						hstart = HStart,
						start = Start },
				    scan_part(Desc1,Bound,C)
			    end
		    end;
		false ->
		    C = #mime { type = "text/info",
				headers = Hs, 
				hstart = HStart,
				start = Start },
		    scan_part(Desc1,Bound,C)
	    end;
	Error -> Error
    end.

%%
%% Scan until Boundary found either Boundary or Boundary-- (end)
%%

scan_part(Desc, Bound, C) ->
    ?dbg("scan_parts: ~s\n", [binary_to_list(Bound)]),
    case scan_boundary(Desc, Bound) of
	{ok,Desc1, <<"--">>} ->
	    {ok,Offset} = position(Desc1),
	    C1 = C#mime { stop = Offset-size(Bound)-2-1 },
	    {ok,Desc1,C1};
	{ok,Desc1, _} ->
	    {ok,Offset} = position(Desc1),
	    C1 = C#mime { stop = Offset-size(Bound)-1 },
	    {more,Desc1,C1};
	{eof,Desc1} ->
	    {ok,Offset} = position(Desc1),
	    C1 = C#mime { stop = Offset },
	    {ok,Desc1,C1}
    end.

%% Collect First part
scan_content_parts(Desc, Bound) ->
    ?dbg("scan_content_parts1: ~s\n", [binary_to_list(Bound)]),
    {ok,Start} = position(Desc),
    case scan_part(Desc, Bound, #mime { type = "text/info",
					encoding = "7bit",
					start = Start }) of
	{ok,Desc1,C} -> {ok,Desc1,[C]};
	{more,Desc1,C} -> scan_content_parts(Desc1, Bound, [C]);
	Error -> Error
    end.

%% Collect all parts 
scan_content_parts(Desc, Bound, List) ->
    ?dbg("scan_content_parts2: ~s\n", [binary_to_list(Bound)]),
    case scan_content(Desc,Bound) of
	{ok,Desc1,Content} ->
	    {ok,Desc1,reverse([Content|List])};
	{more,Desc1,Content} ->
	    scan_content_parts(Desc1,Bound,[Content|List]);
	Error -> Error
    end.


%%
%% Scan to a MIME boundary,
%% Boundary must be a binary
%% return {ok, Desc', RestOfBoundaryLine}
%%
scan_boundary({Fd,_}, <<>>) ->
    {ok,_Pos} = file:position(Fd, {eof,0}),
    {eof, {Fd,<<>>}};
scan_boundary(Desc, Boundary) ->
    scan_boundary(Desc, Boundary, size(Boundary)).

scan_boundary(Desc, Boundary, BSz) ->
    case line(Desc) of
	{ok,Desc1, << Boundary:BSz/binary, Rest/binary >>} ->
	    {ok,Desc1,Rest};
	{ok,Desc1,_Ln} ->
	    scan_boundary(Desc1,Boundary,BSz);
        Other -> Other
    end.


%%
%% Scan headers return {ok,Desc1,Hs}
%%
scan_headers(Desc) ->
    scan_headers(Desc, []).

scan_headers(Desc, Hs) ->
    case line(Desc) of
	{ok,Desc1,Line} -> 
	    scan_headers(Desc1,binary_to_list(Line),Hs);
	{eof,Desc1} -> {ok,Desc1,reverse(Hs)};
	Error -> Error
    end.

scan_headers(Desc, [],Hs)        -> scan_headers(Desc,Hs);
scan_headers(Desc, [$\s|Cs],Hs)  -> scan_headers(Desc, Cs, Hs);
scan_headers(Desc, [$\t|Cs],Hs)  -> scan_headers(Desc, Cs, Hs);
scan_headers(Desc, [$F,$r,$o,$m,$\s|Cs], Hs) ->
    scan_headers(Desc, Cs, [{from, Cs}|Hs]);
scan_headers(Desc, Header, Hs) ->
    case string:chr(Header, $:) of
	0 -> scan_headers(Desc,Hs);
	Ix ->
	    Key = string:substr(Header, 1, Ix-1),
	    Val = string:substr(Header, Ix+1, length(Header)),
	    scan_header(Desc, Key, Val, Hs)
    end.

scan_header(Desc, Key, Val, Hs) ->
    case line(Desc) of
	{eof,Desc1} ->
	    {ok,Desc1,reverse(Hs)};
	{ok,Desc1,Line} ->
	    case binary_to_list(Line) of
		[$\s|Cs] -> scan_header(Desc1, Key, Val++Cs, Hs);
		[$\t|Cs] -> scan_header(Desc1, Key, Val++Cs, Hs);
		[] -> {ok,Desc1,reverse([make_hdr(Key,Val)|Hs])};
		Cs -> scan_headers(Desc1, Cs, [make_hdr(Key,Val)|Hs])
	    end
    end.

make_hdr(Key,[$\s|Val]) -> make_hdr(Key, Val);
make_hdr(Key,[$\t|Val]) -> make_hdr(Key, Val);
make_hdr(Key,Val) ->
    KEY = string:to_upper(Key),
    case KEY of
	"CONTENT-" ++ _What ->
	    Parts =
		map(fun(Part) ->
			    case string:split(Part,$=) of
				[K,V] ->
				    {string:trim(K),string:trim(V)};
				[_] -> 
				    string:trim(Part)
			    end
		    end, tokens(Val, ";")),
	    {list_to_atom(KEY), Parts};
	KEY ->
	    {list_to_atom(KEY),Val}
    end.

%% unquote if double quoted value
unquote(Cs) ->
    unquote(Cs, $", $").

unquote([Quote1|Cs], Quote1, Quote2) ->
    case reverse(Cs) of
	[Quote2|Cs1] -> reverse(Cs1);
	_ -> Cs
    end;
unquote(Cs, _, _) -> Cs.

%%
%% line(Desc) -> 
%%      {ok,Desc',Line} | eof | {error,Reason}
%%
line({Fd,Bin}) ->
    line(Fd,Bin,0).

line(Fd,Bin,Offs) ->
    case Bin of
	<<_:Offs/binary,$\r,$\n,_/binary>> ->
	    <<Line:Offs/binary,_,_,Bin1/binary>> = Bin,
	    {ok,{Fd,Bin1},Line};
	<<_:Offs/binary,$\n,_/binary>> ->
	    <<Line:Offs/binary,_,Bin1/binary>> = Bin,
	    {ok,{Fd,Bin1},Line};
	_ ->
	    if size(Bin) == Offs ->
		    case file:read(Fd, 64) of
			{ok,Bin1} -> 
			    line(Fd,<<Bin/binary,Bin1/binary>>,Offs);
			eof -> {eof,{Fd,<< >> }};
			Error -> Error
		    end;
	       true ->
		    line(Fd,Bin,Offs+1)
	    end
    end.

%% position in file (taking Bin buffer into account)
position({Fd,Bin}) ->
    case file:position(Fd, cur) of
	{ok,Pos} -> {ok,Pos - size(Bin)};
	Error  -> Error
    end.

%%
%% Assign names to content parts
%%
name_content(Content) ->
    name_content("part", Content).

name_content(Name, C = #mime { headers=Hs, parts=SubContent }) ->
    if Hs =/= [], SubContent == [] ->
	    name(C, Name);
       true ->
	    SubContent1 = name_content_list(1, Name, SubContent),
	    case lists:keysearch('CONTENT-TYPE', 1, Hs) of
		{value,{_,[Type|_Params]}} ->
		    MimeType = string:to_lower(Type),
		    C#mime { type = MimeType, 
				parts = SubContent1 };
		false ->
		    C#mime { parts =  SubContent1 }
	    end
    end.

name_content_list(I, Name, [Content|Cs]) ->
    [name_content(Name++"_"++integer_to_list(I), Content) |
     name_content_list(I+1, Name, Cs)];
name_content_list(_, _Name, []) ->
    [].

name(C, BaseName) ->
    C1 = case lists:keysearch('CONTENT-ID', 1, C#mime.headers) of
	     {value,{_, [CID]}} ->
		 C#mime { cid = unquote(CID,$<,$>) };
	     false ->
		 C
	 end,
    C2 = case lists:keysearch('CONTENT-TYPE', 1, C#mime.headers) of
	     {value,{_,[_Type|Params]}} ->
		 case lists:keysearch("name", 1, Params) of
		     {value, {_, QName}} ->
			 Name = unquote(QName),
			 Extension = filename:extension(Name),
			 C1#mime { name = Name,
				      filename = BaseName ++ Extension };
		     false  ->
			 case lists:keysearch('CONTENT-DISPOSITION', 1,
					      C#mime.headers) of
			     {value, {_, DParams}} ->
				 case lists:keysearch("filename", 1, DParams) of
				     {value,{_, QName}} ->
					 Name = unquote(QName),
					 Extension = filename:extension(Name),
					 C1#mime { name = Name,
						      filename = BaseName ++
						      Extension };
				     false ->
					 Extension = 
					     mime_extension(C1#mime.type),
					 C1#mime { filename = BaseName ++
						      Extension }
				 end;
			     false ->
				 Extension = 
				     mime_extension(C1#mime.type),
				 Name = BaseName ++ Extension,
				 C1#mime { name = Name,
					      filename = Name }
			 end
		 end;
	     false ->
		 C1#mime { filename = BaseName }
	 end,
    C3 = case lists:keysearch('CONTENT-TRANSFER-ENCODING', 1, 
			      C#mime.headers) of
	     {value, {_, [Encoding]}} ->
		 C2#mime { encoding = string:to_lower(Encoding) };
	     _ ->
		 C2
	 end,
    C3.

%%
%% Generate a mapping table from Cid -> FileName
%%
cid_map(#mime { cid=Cid, filename=FileName, parts=Content }, Dir) ->
    if Cid == undefined ->
	    cid_map_list(Content, Dir);
       FileName == undefined ->
	    cid_map_list(Content, Dir);
       true ->
	    [{Cid,filename:join(Dir,FileName)} | cid_map_list(Content,Dir)]
    end.

cid_map_list([Content|Cs], Dir) ->
    cid_map(Content, Dir) ++ cid_map_list(Cs, Dir);
cid_map_list([],_) ->
    [].

%% Construct a binary with content for the main.inc
main_content(CidMap, Fd, WebDir, C = #mime { type=Type }, Mbox) ->
    case Type of
	"multipart/related" ->
	    case select_type(C, "text/html") of
		false ->
		    case select_type(C, "multipart/alternative") of
			false ->
			    case select_type(C, "text/plain") of
				false ->
				    <<>>;
				{value,Main} ->
				    main_data(CidMap, WebDir, Fd, Main, Mbox)
			    end;
			{value, Alt} ->
			    main_content(CidMap,Fd,WebDir,Alt,Mbox)
		    end;
		{value, CHtml} ->
		    main_data(CidMap, WebDir,  Fd, CHtml, Mbox)
	    end;

	"multipart/alternative" ->
	    case select_type(C, "text/html") of
		false ->
		    case select_type(C, "multipart/related") of
			false ->
			    case select_type(C, "text/plain") of
				false ->
				    <<>>;
				{value,Main} ->
				    main_data(CidMap, WebDir, Fd, Main, Mbox)
			    end;
			{value, Rel} ->
			    main_content(CidMap,Fd,WebDir,Rel,Mbox)
		    end;

		{value, CHtml} ->
		    main_data(CidMap, WebDir,  Fd, CHtml, Mbox)
	    end;

	"multipart/signed" ->
	    %% remove the signature
	    %% Fixme match the protocol parameter instead
	    Parts = filter(
		      fun(D) when D#mime.type == 
				  "application/x-pkcs7-signature" ->
			      false;
			 (_) -> 
			      true
		      end, C#mime.parts),
	    map(fun(Part) ->
			main_content(CidMap,Fd,WebDir,Part,Mbox)
		end, Parts);

	"multipart/" ++ _ ->
	    map(fun(Part) ->
			main_content(CidMap,Fd,WebDir,Part,Mbox)
		end, C#mime.parts);

	"message/rfc822" ->
	    map(fun(Part) ->
			main_content(CidMap,Fd,WebDir,Part,Mbox)
		end, C#mime.parts);

	_Other ->
	    main_data(CidMap, WebDir, Fd, C, Mbox)
    end.


select_type(#mime { parts=SubContent }, Type) ->
    lists:keysearch(Type, #mime.type, SubContent).

main_data(CidMap, WebDir, Fd, C, Mbox) ->
    case C#mime.type of
	"text/plain" ->
	    case load_content(Fd, Mbox, C) of
		{eror,_Error} ->
		    <<>>;
		{ok, TextBin} ->
		    TextHtml = html_pcdata(binary_to_list(TextBin)),
		    list_to_binary(TextHtml)
	    end;
	"text/html" ->
	    case load_content(Fd, Mbox, C) of
		{eror,_Error} ->
		    <<>>;
		{ok, Html} ->
		    case catch parse_html(Html) of
			{error,_Error} ->
			    <<>>;
			{ok,Document} ->
			    case remap_doc(Document,CidMap) of
				[{html,_,[{head,_,_},{body,As,Cs}]}] ->
				    make_flow(As, Cs);
				[{head,_,_},{body,As,Cs}] ->
				    make_flow(As, Cs);
				[{body,As,Cs}] ->
				    make_flow(As, Cs);
				Cs ->
				    make_flow([], Cs)
			    end
                    end
	    end;
	"image/"++_IMG ->
	    make_img(C,WebDir);
	_ ->
	    make_anchor(C,WebDir)
    end.

make_flow([], Flow) ->
    list_to_binary(html_format(Flow));
make_flow(Attr, Flow) ->
    %% Map body attributes into table attributes
    %% FIXME filter some attributes
    Flow1 = [{table,Attr,
	      [{tr,[],[
		       {td,[],Flow}
		      ]}]}],
    list_to_binary(html_format(Flow1)).


make_img(_C = #mime { filename = File }, WebDir) ->
    if File == "" ->
	    <<>>;
       true ->
	    list_to_binary([ "<img src=\"", filename:join(WebDir, File), "\">" ])
    end.

make_anchor(_C = #mime { filename = Filename, name=Name }, WebDir) ->
    if Filename == "" ->
	    <<>>;
       Name == "" ->
	    list_to_binary([ "<a href=\"", filename:join(WebDir,Filename),"\">",
			     html_pcdata(Filename), 
			     "</a>" ]);
       true ->
	    list_to_binary([ "<a href=\"", filename:join(WebDir,Filename),"\">",
			     html_pcdata(Name),
			     "</a>" ])
    end.



%% Load and decode content
load_content(Fd, Mbox,#mime { encoding=Encoding, start=Start, stop=Stop}) ->
    case file:pread(Fd, Start, Stop-Start) of
	{ok,Bin} ->
	    {ok,decode_content(Encoding,Mbox,Bin)};
	Error ->
	    Error
    end.

save_content(OutDir, CidMap, Fd, 
	     C = #mime { headers=Hs, start=Start, stop=Stop,
			    parts=SubContent }, Mbox) ->
    if Hs =/= [], SubContent == [] ->
	    case file:pread(Fd, Start, Stop-Start) of
		{ok,Bin} ->
		    ?dbg("FILENAME=~p, CID=~p, ENCODING=~p TYPE=~p SIZE=~p\n",
			[C#mime.filename,
			C#mime.cid,
			C#mime.encoding,
			C#mime.type,
		        size(Bin)]),
		    save(OutDir,CidMap,C,Mbox,Bin)
	    end;
       true ->
	    foreach(fun(Cnt) -> 
			    save_content(OutDir,CidMap,Fd,Cnt,Mbox)
		    end, 
		    SubContent)
    end.

save(OutDir,CidMap,C,Mbox,Bin) ->
    FileName = filename:join(OutDir, C#mime.filename),
    Decoded  = decode_content(C#mime.encoding, Mbox, Bin),
    Translated = case C#mime.type of
		     "text/html" -> remap_cid(Decoded, CidMap);
		     _Other -> Decoded
		 end,
    file:write_file(FileName, Translated).

remap_cid(Bin, CidMap) ->
    case catch parse_html(Bin) of
	{ok,Document} ->	    
	    Content = remap_html(makeup:get_content(Document), CidMap),
	    Document1 = makeup:set_content(Document,Content),
	    iolist_to_binary(html_format(Document1));
	Error ->
	    ?dbg("remap_cid: Error ~p\n", [Error]),
	    Bin
    end.

remap_doc(Document, CidMap) ->
    Content = makeup:get_content(Document),
    remap_html(Content,CidMap).
%%
%% remap values on form "cid:<tag>" to filename
%%
remap_html({Tag,As,Children}, CidMap) ->
    {Tag,remap_as(As,CidMap), remap_html(Children,CidMap)};
remap_html({Tag,As}, CidMap) ->
    {Tag,remap_as(As,CidMap)};
remap_html([H|T], CidMap) ->
    [remap_html(H,CidMap) | remap_html(T, CidMap)];
remap_html([], _CidMap) ->
    [];
remap_html(_Other, CidMap) ->
    CidMap.

remap_as([KV={Key,"cid:"++Cid}|As], CidMap) ->
    case lists:keysearch(Cid, 1, CidMap) of
	{value, {_, Value}} ->
	    [{Key,Value}|remap_as(As, CidMap)];
	false ->
	    [KV|remap_as(As, CidMap)]
    end;
remap_as([KV|As], CidMap) ->
    [KV|remap_as(As, CidMap)];
remap_as([], _) ->
    [].
    
	    
mime_extension(Type) ->
    case mime_types:lookup_type(Type) of
	[Ext|_] -> "."++Ext;
	[] -> "";
	false -> ""
    end.

decode_content(undefined, false, Bin) ->
    Bin;
decode_content(undefined, true, Bin) ->
    list_to_binary(decode_mbox(binary_to_list(Bin)));
decode_content("7bit", false, Bin) ->
    Bin;
decode_content("7bit", true, Bin) ->
    list_to_binary(decode_mbox(binary_to_list(Bin)));
decode_content("quoted-printable", _Mbox, Bin) ->
    list_to_binary(decode_qp(binary_to_list(Bin)));
decode_content("base64", _Mbox, Bin) ->
    list_to_binary(decode_b64(binary_to_list(Bin)));
decode_content("8bit", _Mbox, Bin) ->
    Bin;
decode_content("binary",_Mbox,Bin) ->
    Bin.

%% decode quoted printable
decode_qp([$=,$\r,$\n|Rest]) -> decode_qp(Rest);
decode_qp([$=,$\n|Rest]) -> decode_qp(Rest);
decode_qp([$=,H1,H2|Rest]) -> [(hex(H1) bsl 4 + hex(H2))|decode_qp(Rest)];
decode_qp([C|Cs]) -> [C|decode_qp(Cs)];
decode_qp([]) -> [].

%% decode base64
decode_b64([$\s|Cs]) -> decode_b64(Cs);
decode_b64([$\t|Cs]) -> decode_b64(Cs);
decode_b64([$\n|Cs]) -> decode_b64(Cs);
decode_b64([$\r|Cs]) -> decode_b64(Cs);
decode_b64([A,B,$=,$=|_]) ->
    <<E,_,_>> = <<(d(A)):6,(d(B)):6,0:6,0:6>>,
    [E];
decode_b64([A,B,C,$=|_]) ->
    <<E,F,_>> = <<(d(A)):6,(d(B)):6,(d(C)):6,0:6>>,
    [E,F];
decode_b64([A,B,C,D|Cs]) ->
    <<E,F,G>> = <<(d(A)):6,(d(B)):6,(d(C)):6,(d(D)):6>>,
    [E,F,G|decode_b64(Cs)];
decode_b64([]) ->
    [].

%%
%% Decode mbox quotes i.e >From => From
%%
decode_mbox([$>|Cs]) -> decode_from(Cs,[$>]);
decode_mbox(Cs) -> decode_mbx(Cs).

decode_mbx([$\n,$>|Cs]) -> decode_from(Cs,[$>]);
decode_mbx([C|Cs]) -> [C|decode_mbx(Cs)];
decode_mbx([]) -> [].

decode_from([$>|Cs],Acc) -> decode_from(Cs,[$>|Acc]);
decode_from([$F,$r,$o,$m,$\s|Cs],Acc) -> tl(Acc)++"From "++decode_mbx(Cs);
decode_from(Cs,Acc) -> Acc++decode_mbx(Cs).

    


d(X) when X >= $A, X =< $Z -> X - $A;
d(X) when X >= $a, X =< $z -> (X - $a)+26;
d(X) when X >= $0, X =< $9 -> (X - $0)+52;
d($+) -> 62;
d($/) -> 63.
    

hex(X) when X >= $0, X =< $9 -> (X-$0);
hex(X) when X >= $A, X =< $F -> (X-$A)+10;
hex(X) when X >= $a, X =< $f -> (X-$a)+10.

%%
display_content(C) ->
    display_content(C,0).

display_content(C, I) ->
    Indent = indent(I),
    Start = C#mime.start,
    Stop = C#mime.stop,
    Size = if is_integer(Start), is_integer(Stop) -> 
		   integer_to_list(Stop-Start);
	      is_integer(Start) ->
		   integer_to_list(Start) ++ "-";
	      is_integer(Stop) ->
		   "-"++integer_to_list(Start);
	      true ->
		   "-"
	   end,
    io:format("~s~p file=~p size=~s\n",
	      [Indent, C#mime.type, C#mime.filename, Size]),
    foreach(fun(D) -> display_content(D,I+1) end, 
	    C#mime.parts).

indent(N) ->
    lists:duplicate(N*4, $\s).

parse_html(Data) ->
    makeup:string(iolist_to_binary(Data),[{strict,false}]).

html_format(Flow) when is_list(Flow) ->
    makeup:format(Flow, [{content_type,"text/html"}]).

%% replace multiple white space with one space,
%% remove inital and final white space
%% then format as {'#PCDATA', Text}
html_pcdata(Text) when is_list(Text) ->
    makeup:format({'#PCDATA', trim_text(Text)},[{content_type,"text/html"}]).

trim_text(Cs) ->
    trim_text_bl0(Cs).

%% skip all leading blanks
trim_text_bl0([C|Cs]) when C =< 32 -> trim_text_bl0(Cs);
trim_text_bl0(Cs) -> trim_collect(Cs).

%% skip all but one blanks (unless last)
trim_text_bl1([C|Cs]) when C =< 32 -> trim_collect(Cs);
trim_text_bl1([]) -> [];
trim_text_bl1(Cs) -> [$\s|trim_text(Cs)].

%% collect word
trim_collect([C|Cs]) when C =< 32 -> trim_text_bl1(Cs);
trim_collect([C|Cs]) -> [C|trim_collect(Cs)];
trim_collect([]) -> [].
