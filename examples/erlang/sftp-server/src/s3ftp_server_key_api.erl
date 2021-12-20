-module(s3ftp_server_key_api).

-behaviour(ssh_server_key_api).

%% API
-export([host_key/2, is_auth_key/3]).


host_key(Algorithm, DaemonOptions) ->
  lager:info("s3ftp_server_key_api:host_key(~p,~p): not_supported", [Algorithm, DaemonOptions]),
  {error, not_supported}.

is_auth_key(Key, User, DaemonOptions) ->
  lager:info("s3ftp_server_key_api:is_auth_key(~p,~p,~p): not_supported", [Key, User, DaemonOptions]),
  false.
