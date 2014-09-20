-module(s3ftp_fserver).

-behaviour(ssh_sftpd_file_api).

-include_lib("kernel/include/file.hrl").

%% API
-export([close/2, delete/2, del_dir/2, get_cwd/1, is_dir/2, list_dir/2, 
	 make_dir/2, make_symlink/3, open/3, position/3, read/3,
	 read_file_info/2, read_link/2, read_link_info/2, rename/3,
	 write/3, write_file_info/3]).

close(IoDevice, State) ->
    lager:info("s3ftp_fserver:close: not_supported", []),
    {{error, not_supported}, State}.

delete(Path, State) ->
    lager:info("s3ftp_fserver:delete: not_supported", []),
    {{error, not_supported}, State}.

del_dir(Path, State) ->
    lager:info("s3ftp_fserver:del_dir: not_supported", []),
    {{error, not_supported}, State}.

get_cwd(State) ->
    lager:info("s3ftp_fserver:get_cwd: returning hard-coded result", []),
    {{ok, "your-s3-bucket/path-but-not-file"}, State}.

is_dir(AbsPath, State) ->
    lager:info("s3ftp_fserver:is_dir: not_supported", []),
    {true, State}.

list_dir(AbsPath, State) -> 
    lager:info("s3ftp_fserver:list_dir: not_supported", []),
  {{ok, ["something", "goes", "here"]},State}.
     
make_dir(Dir, State) ->
    lager:info("s3ftp_fserver:make_dir: not_supported", []),
    {{error, not_supported}, State}.
     
make_symlink(Path2, Path, State) ->
    lager:info("s3ftp_fserver:make_symlink: not_supported", []),
    {{error, not_supported}, State}.

open(Path, Flags, State) ->
    lager:info("s3ftp_fserver:open: not_supported", []),
    {{error, not_supported}, State}.
     
position(IoDevice, Offs, State) ->
    lager:info("s3ftp_fserver:position: not_supported", []),
    {{error, not_supported}, State}.

read(IoDevice, Len, State) ->
    lager:info("s3ftp_fserver:read: not_supported", []),
    {{error, not_supported}, State}.
          
read_link(Path, State) ->
    lager:info("s3ftp_fserver:read_link: returning einval", []),
    {{error, einval}, State}.

read_link_info(Path, State) ->
    lager:info("s3ftp_fserver:read_link_info: not_supported", []),
    {{ok, #file_info{size=0}}, State}.
     
read_file_info(Path, State) ->
    lager:info("s3ftp_fserver:read_file_info: not_supported", []),
    {{error, not_supported}, State}.

rename(Path, Path2, State) ->
    lager:info("s3ftp_fserver:rename: not_supported", []),
    {{error, not_supported}, State}.

write(IoDevice, Data, State) ->
    lager:info("s3ftp_fserver:write: not_supported", []),
    {{error, not_supported}, State}.
     
write_file_info(Path,Info, State) ->
    lager:info("s3ftp_fserver:write_file_info: not_supported", []),
    {{error, not_supported}, State}.
