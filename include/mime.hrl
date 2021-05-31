%%
%% Mime 
%%
-record(mime,
	{
	  type,            %% content mime type
	  headers = [],    %% Message and mime headers
	  hstart,          %% Start offset for headers
	  start,           %% Start offset in file
	  stop,            %% Stop offset in file
	  name = "",       %% orginal disposition name
	  filename = "",   %% generate filename
	  cid,             %% cid (content id)
	  encoding,        %% Transfer encoding
	  parts = []       %% multi parts
	 }).
