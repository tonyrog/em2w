%%% File    : mime_gen.erl
%%% Author  : Tony Rogvall <tony@bix.hemma.se>
%%% Description : Generate files
%%% Created : 11 Mar 2003 by Tony Rogvall <tony@bix.hemma.se>

-module(mime_gen).

-export([types/1]).
-import(lists, [map/2]).

types([]) ->
    types(['/etc/mime.types']);
types([File]) ->
    case file:read_file(atom_to_list(File)) of
	{ok, Bin} ->
	    Types = load_types(Bin),
	    gen_mime_types(Types),
	    halt(0);
	Error ->
	    io:format("mime_gen: error ~p\n", [Error]),
	    halt(1)
    end.

gen_mime_types(Types) ->
    H = [ "-module(mime_types).\n",
	  "-export([lookup_type/1, lookup_ext/1]).\n",
	  "\n"],
    LT = 
	[ map(fun({Type,[E|Exts]}) ->
		    ["lookup_type(",$",Type,$",") -> [",
		     "\"",E,"\"",map(fun(Ex) -> [",",$",Ex,$"] end, Exts),
		     "];\n"];
	       ({Type,[E]}) ->
		    ["lookup_type(",$",Type,$",") -> [",$",E,$",
		     "];\n"];
	       ({Type,[]}) ->
		    ["lookup_type(",$",Type,$",") -> [",
		     "];\n"]
	    end, Types),
	  "lookup_type(_) -> false.\n"],
    LE = 
	[ map(fun({Type,Exts}) ->
		      map(fun(E) -> 
				  ["lookup_ext(",$",E,$",") -> ",
				   $",Type,$",";\n"]
			  end,Exts)
	      end, Types),
	  "lookup_ext(_) -> false.\n"],
    file:write_file("mime_types.erl",[H,LT,"\n",LE,"\n"]).


load_types(Bin) ->
    lists:foldl(
      fun(Line,Acc) ->
	      case string:tokens(Line,"\s\t") of
		  ["#"++_|_] -> Acc;
		  [MimeType|Exts] -> [{MimeType,Exts}|Acc]
	      end
      end, [], string:tokens(binary_to_list(Bin), "\n\r")).






