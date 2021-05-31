%%% File    : em2w.erl
%%% Author  :  <em2w@bix.hemma.se>
%%% Description : em2w deliver program
%%% Created : 11 Mar 2003 by  <em2w@bix.hemma.se>

-module(em2w).

-export([command/1]).

-include_lib("kernel/include/file.hrl").


local_dir() -> ["/", "home", "em2w", "outgoing"].

site_dir(Site) -> local_dir() ++ [Site].

page_dir(Site,Page) -> local_dir() ++ [Site,Page].

item_dir(Site,Page,Item) -> local_dir() ++ [Site,Page,Item].

config_file() -> "/home/em2w/.config".

%%
%% Read the site configuration
%%  {Web-Site, Site-Dir, Ftp-Site, Ftp-User, Ftp-Password, Ftp-Dir}
%%
site_config(Site) ->
    case file:consult(config_file()) of
	{ok,SiteList} ->
	    case lists:keysearch(Site, 1, SiteList) of
		{value, SiteConfig} ->
		    {ok,SiteConfig};
		false ->
		    {error, not_found}
	    end;
	Error ->
	    Error
    end.
%%
%% Read item state
%%
%%  {parts, ISet}
%%  {flow, Main-Part1, Used-Parts1, File-Parts1}
%%  {flow, Main-Part2, Used-Parts1, File-Parts1}
%%  ...
%%  {flow, Main-PartN, Used-PartsN, File-PartsN}
%%
item_state(Item) ->
    ok.
    

ftp_files(Ftp, Dir) ->
    case file:list_dir(Dir) of
	{ok, List} ->
	    lists:foreach(fun(File) ->
				  ftp:send(Ftp, filename:join(Dir,File), File)
			  end, List);
	Error ->
	    Error
    end.
    
ftp_deliver(Dir, FtpDir, FtpHost, FtpUser, FtpPass) ->
    case ftp:open(FtpHost,[verbose]) of
	{ok,Ftp} ->
	    case ftp:user(Ftp, FtpUser, FtpPass) of
		ok ->
		    case ftp:cd(Ftp, FtpDir) of
			ok ->
			    Res = ftp_files(Ftp, Dir),
			    ftp:close(Ftp),
			    Res;
			Error ->
			    ftp:close(Ftp),
			    Error
		    end;
		Error ->
		    ftp:close(Ftp),
		    Error
	    end;
	Error -> Error
    end.

%%
%% sync file on the ftp server in current directory
%% with the local file
%%

site_deliver(File, OutDir, Page, Item, Config) ->
    {Site, SiteDir, FtpHost, FtpUser, FtpPass, FtpDir } = Config,
    case mime:unpack_em2w(File, OutDir,
			  filename:join([SiteDir,Page,Item])) of
	{ok,_} ->
	    ftp_deliver(OutDir, filename:join([FtpDir,Page,Item]),
			FtpHost, FtpUser, FtpPass);
	Error ->
	    Error
    end.

%%
%% Assert that a directory exist
%%
assert_directory(Dir) ->
    case file:read_file_info(Dir) of
	{ok,Info} when Info#file_info.type == directory ->
	    ok;
	_ ->
	    throw({error, {assertion_failed,director,Dir}})
    end.

%%
%% Create a new page
%%
make_page_dir(Site, Page) ->
    file:make_dir(filename:join(page_dir(Site,Page))).

make_item_dir(Site, Page, Item) ->
    file:make_dir(filename:join(item_dir(Site,Page,Item))).

%%
%%         delete  /site/page/item[/subitem]
%%
%%              Delete the subitem, delete all subitems if no subitem is given.
%%
em2w_delete([Site,Page,Item|SubItem], File) ->
    ok.

%%
%%         replace /site/page/item[/subitem]
%%
%%              Replace subitem with new data, will delete all content
%%              and insert a fresh flow if no subitem number is given
%%
em2w_replace([Site,Page,Item|SubItem], File) ->
    OutDir = filename:join(item_dir(Site,Page,Item)),
    assert_directory(OutDir),
    case site_config(Site) of
	{ok, Config} ->
	    site_deliver(File, OutDir, Page, Item, Config);
	Error ->
	    Error
    end;
em2w_replace(_, File) ->
    {error, bad_arg}.
    
%%
%%         insert  /site/page/item[/subitem]
%%
%%              Insert subitem before [subitem], if subitem is not given
%%              then this command will insert after the last
%%              subitem (i. append)
%%
em2w_insert([Site,Page,Item|SubItem], File) ->
    ok.

%%
%%         append  /site/page/item
%%
%%              Append a subitem to the item
%%
em2w_append([Site,Page,Item|SubItem], File) ->
    ok.

%%
%%         prepend /site/page/item
%%
%%              Prepend a flow to the item
%%
em2w_prepend([Site,Page,Item|SubItem], File) ->
    ok.

%%
%% admin commands
%%

%%
%%         flows /site/page/item/max-subitems
%%
%%             Set maximum number of flows for a given item
%%             (default=unlimited)
%%
em2w_flows([Site,Page,Item|MaxSubItems], File) ->
    ok.

%%
%%         sync /site[/page[/item]]
%%
%%              Sync the item files with the ftp site,
%%              sync all site files or page files or
%%              item files.
%%
em2w_sync([Site,Page,Item], File) ->
    ok;
em2w_sync([Site,Page], File) ->
    ok;
em2w_sync([Site], File) ->
    ok;
em2w_sync(_, File) -> 
    {error, bad_arg}.

%%
%%         add /site/page[/item]
%%
%%              Add a page/item to the site
%%
em2w_add([Site,Page], File) ->
    ok;
em2w_add([Site,Page,Item], File) ->
    ok;
em2w_add(_, File) ->
    {error, bad_arg}.

%%
%%         del /site/page[/item]
%%
%%              Remove a page/item from the site
%%
em2w_del([Site,Page], File) ->
    ok;
em2w_del([Site,Page,Item], File) ->
    ok;
em2w_del(_, File) ->
    {error, bad_arg}.

%%
%% Command dispatcher
%%
%% usage:  <command> <object> <file>
%%
%%
command([Command, Obj, File, Sender, Verified]) ->
    io:format("sender = ~p, verified=~p\n", [Sender, Verified]),
    %% FIXME add error handling
    Object = string:tokens(atom_to_list(Obj),"/"),
    FileName = atom_to_list(File),
    Result = (
      catch begin
		if Command == replace; Command == r ->
			em2w_replace(Object, FileName);
		   Command == delete; Command == d ->
			em2w_delete(Object, FileName);
		   Command == insert; Command == i ->
			em2w_insert(Object, FileName);
		   Command == append; Command == a ->
			em2w_append(Object, FileName);
		   Command == prepend; Command == p ->
			em2w_prepend(Object, FileName);
		    %%
		    %% admin stuff
		    %%
		   Command == flows ->
			em2w_flows(Object, FileName);
		   Command == sync ->
			em2w_sync(Object, FileName);
		   Command == add ->
			em2w_add(Object, FileName);
		   Command == del ->
			em2w_del(Object, FileName);
		   true ->
			{error, illegal_command}
		  end
	    end),

    case Result of
	ok ->
	    halt(0);

	{'EXIT', Reason} ->
	    io:format("em2w: crash: ~p\n", [Reason]),
	    halt(1);

	Error ->
	    io:format("em2w: error: ~p\n", [Error]),
	    halt(1)
    end.


    
    
