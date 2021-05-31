%%% File    : em2w.hrl
%%% Author  :  <em2w@bix.hemma.se>
%%% Description : record structures
%%% Created : 17 Mar 2003 by  <em2w@bix.hemma.se>

-record(site,
	{
	  id,
	  url,        %% http://host..
	  url_dir,    %% /dir
	  ftp_host,   %% host or ip-address
	  ftp_user,   %% user at ftp host
	  ftp_pass,   %% user password
	  ftp_dir,    %% /upload-dir
	  max_mesg,   %% max message size accepted
	  max_pages,  %% max number of pages for this site
	  max_items,  %% max items per page
	  user = []   %% user acl 
	 }).

-record(user,
	{
	  id,                %% email address pattern u@d | *@d | *
	  admin = false,     %% true, false
	  verify = require,  %% require, on, off
	  content = []
	 }).

-record(content,
	{
	  id,               %% content type major/minor,major/* or */* 
	  max_size,         %% max size of this content type
	  verify = off,     %% verify content on|off
	  filter            %% content filter
	 }).
	  


