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
    lager:info("s3ftp_fserver:get_cwd(~p): returning hard-coded result", [State]),
    {{ok, "your-s3-bucket/path-but-not-file"}, State}.

is_dir(AbsPath, State) ->
    lager:info("s3ftp_fserver:is_dir(~p,~p): not_supported", [AbsPath, State]),
    {true, State}.

list_dir(AbsPath, State) -> 
    lager:info("s3ftp_fserver:list_dir(~p,~p): not_supported", [AbsPath, State]),
  {{ok, ["something", "goes", "here"]},State}.
     
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
    lager:info("s3ftp_fserver:read_link_info(~p,~p): returning file size of 0", [Path, State]),
    {{ok, #file_info{size=0}}, State}.
     
read_file_info(Path, State) ->
    lager:info("s3ftp_fserver:read_file_info(~p,~p): not_supported", [Path, State]),
    {{error, not_supported}, State}.

rename(Path, Path2, State) ->
    lager:info("s3ftp_fserver:rename(~p,~p,~p): not_supported", [Path, Path2, State]),
    {{error, not_supported}, State}.

write(IoDevice, Data, State) ->
    lager:info("s3ftp_fserver:write(~p,~p,~p): not_supported", [IoDevice, Data, State]),
    {{error, not_supported}, State}.
     
write_file_info(Path, Info, State) ->
    lager:info("s3ftp_fserver:write_file_info(~p,~p),~p: not_supported", [Path, Info, State]),
    {{error, not_supported}, State}.

