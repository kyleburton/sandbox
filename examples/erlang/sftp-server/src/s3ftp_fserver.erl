-module(s3ftp_fserver).

-behaviour(ssh_sftpd_file_api).

-include_lib("kernel/include/file.hrl").

%% API
-export([close/2, delete/2, del_dir/2, get_cwd/1, is_dir/2, list_dir/2, 
	 make_dir/2, make_symlink/3, open/3, position/3, read/3,
	 read_file_info/2, read_link/2, read_link_info/2, rename/3,
	 write/3, write_file_info/3]).

close(IoDevice, State) ->
    lager:info("s3ftp_fserver:close(~p,~p): not_supported", [IoDevice, State]),
    {{error, not_supported}, State}.

delete(Path, State) ->
    lager:info("s3ftp_fserver:delete(~p,~p): not_supported", [Path, State]),
    {{error, not_supported}, State}.

del_dir(Path, State) ->
    lager:info("s3ftp_fserver:del_dir(~p,~p): not_supported", [Path, State]),
    {{error, not_supported}, State}.

get_cwd(State) ->
    Path = "/",
    lager:info("s3ftp_fserver:get_cwd(~p): returning Path=~p", [State, Path]),
    % NB: assoc cwd into State
    {{ok, Path}, State}.

is_dir(AbsPath, State) ->
    User = "kburton",
    S3Path = lists:concat(["s3ftp/", User, "/dropoff", AbsPath]),
    lager:info("s3ftp_fserver:is_dir(~p,~p): S3Path=~s", [AbsPath, State, S3Path]),
    try
      % NB: get_object_metadata throws
      _ = erlcloud_s3:get_object_metadata("rn-dev-sandbox", S3Path),
      lager:info("s3ftp_fserver:is_dir: got metadata, not a dir", []),
      {false, State}
    catch
      error:Reason ->
        lager:info("s3ftp_fserver:is_dir: exception getting metadata, _is_ a dir: ~p", [Reason]),
        {true, State}
    end.

list_dir(AbsPath, State) -> 
    lager:info("s3ftp_fserver:list_dir(~p,~p): trying s3...", [AbsPath, State]),
    % NB: need to pull the username from State, then concat them together to form Prefix
    User = "kburton",
    Prefix = lists:concat(["s3ftp/", User, "/dropoff", AbsPath]),
    lager:info("s3ftp_fserver:list_dir: listing at prefix: ~s :: ~p", [Prefix, Prefix]),
    Resp = erlcloud_s3:list_objects("rn-dev-sandbox",[{prefix, Prefix}]),
    Contents = proplists:get_value(contents,Resp),
    lager:info("s3ftp_fserver:list_dir: s3 Resp=~p", [Resp]),
    FileNames = lists:map(fun (C) -> 
                              P = proplists:get_value(key,C),
                              Fname = lists:nthtail(length(Prefix), P),
                              lager:info("s3ftp_fserver:list_dir: removing prefix: ~p:~s from ~p:~s => ~s", [length(Prefix), Prefix, length(P), P, Fname]),
                              Fname
                          end,
                          Contents),
    lager:info("s3ftp_fserver:list_dir(~p,~p): FileNames=~p", [AbsPath, State, FileNames]),
    % {{ok, ["something", "goes", "here"]},State}.
    {{ok, FileNames},State}.
     
make_dir(Dir, State) ->
    lager:info("s3ftp_fserver:make_dir(~p,~p): not_supported", [Dir, State]),
    {{error, not_supported}, State}.
     
make_symlink(Path2, Path, State) ->
    lager:info("s3ftp_fserver:make_symlink(~p,~p,~p): not_supported", [Path2, Path, State]),
    {{error, not_supported}, State}.

open(Path, Flags, State) ->
    lager:info("s3ftp_fserver:open(~p,~p,~p): not_supported", [Path, Flags, State]),
    {{error, not_supported}, State}.
     
position(IoDevice, Offs, State) ->
    lager:info("s3ftp_fserver:position(~p,~p,~p): not_supported", [IoDevice, Offs, State]),
    {{error, not_supported}, State}.

read(IoDevice, Len, State) ->
    lager:info("s3ftp_fserver:read(~p,~p,~p): not_supported", [IoDevice, Len, State]),
    {{error, not_supported}, State}.
          
read_link(Path, State) ->
    lager:info("s3ftp_fserver:read_link(~p,~p): returning einval", [Path, State]),
    {{error, einval}, State}.

read_link_info(Path, State) ->
    User = "kburton",
    S3Path = lists:concat(["s3ftp/", User, "/dropoff", Path]),
    lager:info("s3ftp_fserver:read_link_info(~p,~p): returning file info: ~p", [Path, State, S3Path]),
    Resp = erlcloud_s3:get_object_metadata("rn-dev-sandbox", S3Path),
    lager:info("s3ftp_fserver:read_link_info: ~p => ~p", [S3Path, Resp]),
    ContentLengthStr = proplists:get_value(content_length,Resp),
    {ContentLength, _Rest} = string:to_integer(ContentLengthStr),
    {{ok, #file_info{size=ContentLength, type=regular}}, State}.
     
read_file_info(Path, State) ->
    lager:info("s3ftp_fserver:read_file_info(~p,~p): returning file_info record (size)", [Path, State]),
    User = "kburton",
    S3Path = lists:concat(["s3ftp/", User, "/dropoff", Path]),
    Resp = erlcloud_s3:get_object_metadata("rn-dev-sandbox", S3Path),
    ContentLengthStr = proplists:get_value(content_length,Resp),
    {ContentLength, _Rest} = string:to_integer(ContentLengthStr),
    {{ok, #file_info{size=ContentLength, type=regular}}, State}.

rename(Path, Path2, State) ->
    lager:info("s3ftp_fserver:rename(~p,~p,~p): not_supported", [Path, Path2, State]),
    {{error, not_supported}, State}.

write(IoDevice, Data, State) ->
    lager:info("s3ftp_fserver:write(~p,~p,~p): not_supported", [IoDevice, Data, State]),
    {{error, not_supported}, State}.
     
write_file_info(Path, Info, State) ->
    lager:info("s3ftp_fserver:write_file_info(~p,~p),~p: not_supported", [Path, Info, State]),
    {{error, not_supported}, State}.

