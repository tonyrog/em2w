%%% File    : em2w_verify.erl
%%% Author  :  <em2w@localhost.localdomain>
%%% Description : PGP/PKCS7 mail decode and verify
%%% Created : 29 May 2003 by  <em2w@localhost.localdomain>
%%%
-module(em2w_verify).

-compile(export_all).
-import(lists, [reverse/1]).

-include("../include/mime.hrl").

-define(debug, true).

-ifdef(debug).
-define(dbg(Fmt,Args), io:format(Fmt, Args)).
-else.
-define(dbg(Fmt,Args), ok).
-endif.

file(File) ->
    case mime:map_mbox(File) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Mime} ->
	    io:format("Mime: ~p\n", [Mime]),
	    file(File, Mime)
    end.

file(File, Mime) when Mime#mime.type == "multipart/signed" ->
    Mime1 = mime:name_content(Mime),
    case lists:keysearch('CONTENT-TYPE', 1, Mime1#mime.headers) of
	{value, {_, Cs}} ->
	    case lists:keysearch("protocol", 1, Cs) of
		{value,{_,"\"application/pgp-signature\""}} ->
		    verify(File, Mime1, pgp);
		{value,{_,"\"application/x-pkcs7-signature\""}} ->
		    verify(File, Mime1, pkcs7);
		{value, _} ->
		    {error, unsupported_protocol};
		false ->
		    {error, no_protocol}
	    end;
	false ->
	    {error, no_content_type}
    end;
file(File, _) ->
    {error, not_signed}.

%%
%% Detach and verify
%%
verify(File, Mime, Type) ->
    DataFile = File ++ ".dat",
    SigFile = File ++ ".sig",
    case Mime#mime.parts of
	[Info,Signed,Signature] when Info#mime.type == "text/info" ->
	    save_data(File, Signed, DataFile),
	    save_sig(File, Signature, SigFile),
	    Res = verify(Mime, DataFile, SigFile, Type),
	    %% file:delete(DataFile),
	    %% file:delete(SigFile),
	    Res;
	[Signed,Signature] ->
	    save_data(File, Signed, DataFile),
	    save_sig(File, Signature, SigFile),
	    Res = verify(Mime, DataFile, SigFile, Type),
	    %% file:delete(DataFile),
	    %% file:delete(SigFile),
	    Res;
	_ ->
	    {error, bad_parts}
    end.

verify(Mime, DataFile, SigFile, pgp) ->
    %% Verify command
    VfyCmd = "gpg --verify " ++ SigFile ++ " " ++ DataFile ++ 
	" 2>/dev/null",
    io:format("pkcs7: verify command: ~s\n", [VfyCmd]),
    P = open_port({spawn, VfyCmd}, [exit_status]),
    receive
	{P, {exit_status, 0}} ->
	    ok;
	{P, {exit_status, _}} ->
	    {error, bad_signature}
    after 5000 ->
	    {error, timeout}
    end;
verify(Mime, DataFile, SigFile, pkcs7) ->
    %% Verfy command
    VfyCmd = "openssl smime -verify -content " ++ DataFile ++ 
	" -in " ++ SigFile ++ " -inform DER -out /dev/null",
    io:format("pkcs7: verify command: ~s\n", [VfyCmd]),
    P = open_port({spawn,VfyCmd}, [exit_status]),
    receive 
	{P, {exit_status, 0}} ->
	    %% Extract email from cert
	    CertEmail = "openssl pkcs7 -print_certs -inform DER -in " ++ 
		SigFile ++ " | openssl x509 -noout -email",
	    io:format("pkcs7: email command: ~s\n", [CertEmail]),
	    Q = open_port({spawn,CertEmail},[]),
	    receive
		{Q, {data,Email}} ->
		    case lists:keysearch('from', 1, Mime#mime.headers) of
			false ->
			    {error, no_sender};
			{value,{_, Sender}} ->
			    case cequal(trim(Email), trim(Sender)) of
				true ->
				     ok;
				false ->
				    {error, bad_sender}
			    end
		    end
	    after 5000 ->
		    {error, timeout}
	    end;
	{P, {exit_status, _}} ->
	    {error, bad_signature}
    
    after 5000 ->
	    {error, timeout}
    end.



    

save_sig(Src, C, Dst) ->
    #mime { start=Start, stop=Stop } = C,
    case read_content(Src, Start, Stop) of
	{ok, Bin} ->
	    Bin1 = mime:decode_content(C#mime.encoding, true, Bin),
	    file:write_file(Dst, Bin1);	    
	Error ->
	    Error
    end.

save_data(Src, C, Dst) ->
    #mime { hstart=Start, stop=Stop } = C,
    case read_content(Src, Start, Stop) of
	{ok, Bin} ->
	    Bin1 = if C#mime.encoding == "7bit";
		      C#mime.encoding == undefined ->
			   normalize(Bin, true);
		      true ->
			   normalize(Bin, false)
		   end,
	    file:write_file(Dst, Bin1);
	Error ->
	    Error
    end.

trim(Cs) ->
    case string:tokens(Cs, " \n\r\t") of
	[Item|_] -> Item;
	[] -> []
    end.

cequal(A, B) ->
    io:format("~s = ~s\n", [A,B]),
    ceq(A,B).

ceq([C|Cs],[D|Ds]) ->    
    if C == D -> ceq(Cs,Ds);
       true ->
	    case upper(C) of
		D -> ceq(Cs,Ds);
		U ->
		    case upper(D) of
			U -> ceq(Cs,Ds);
			_ -> false
		    end
	    end
    end;
ceq([],[]) -> true.

upper(C) when C >= $a, C =< $z -> (C-$a)+$A;
upper(C) -> C.
	    

%%
%% Normalize pgp data before verify
%% 1. undo mbox quoting >From => From
%% 2. \n => \r\n
%%
normalize(Bin, Mbox) ->
    list_to_binary(norm_pgp(binary_to_list(Bin),[],Mbox)).

norm_pgp([$>|Cs],Acc,true) -> norm_from(Cs,[$>],Acc);
norm_pgp(Cs,Acc,MBox)      -> norm_mbx(Cs,Acc,MBox).

norm_mbx([$\r,$\n,$>|Cs],Acc,true) -> norm_from(Cs,[$>],[$\n,$\r|Acc]);
norm_mbx([$\n,$>|Cs],Acc,true)     -> norm_from(Cs,[$>],[$\n,$\r|Acc]);
norm_mbx([$\r,$\n|Cs],Acc,MBox)    -> norm_mbx(Cs,[$\n,$\r|Acc],MBox);
norm_mbx([$\n|Cs],Acc,MBox)        -> norm_mbx(Cs,[$\n,$\r|Acc],MBox);
norm_mbx([C|Cs],Acc,MBox)          -> norm_mbx(Cs, [C|Acc],MBox);
norm_mbx([], Acc,MBox) -> reverse(Acc).
     
norm_from([$>|Cs],Acc,Acc2) -> norm_from(Cs,[$>|Acc],Acc2);
norm_from([$F,$r,$o,$m,$\s|Cs],Acc,Acc2) ->
    Acc3 = tl(Acc)++Acc2,
    norm_mbx(Cs, [$\s,$m,$o,$r,$F | Acc3], true);
norm_from(Cs,Acc,Acc2) -> 
    norm_mbx(Cs, Acc++Acc2, true).


read_content(Src, Start, Stop) ->
    case file:open(Src, [read,read,binary]) of
	{ok, Fd} ->
	    case file:pread(Fd, Start, (Stop-Start)-1) of
		{ok,Bin} ->
		    file:close(Fd),
		    {ok,Bin};
		Error ->
		    file:close(Fd),
		    Error
	    end;
	Error ->
	    Error
    end.
    

		    
		    
    
					
	    
	



			 
			


			    

						 
	       
	    
