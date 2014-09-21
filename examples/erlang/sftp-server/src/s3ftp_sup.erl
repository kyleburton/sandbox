-module(s3ftp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

no_shell(User) ->
  lager:info("no_shell: sorry, no shell for user=~p", [User]),
  nil.

init([]) ->
    lager:start(),
    R1 = crypto:start(),
    lager:info("init: started crypto, R1=~p", [R1]),
    R3 = ssh:start(),
    lager:info("init: started ssh, R3=~p", [R3]),
    R4 = ssl:start(),
    lager:info("init: started ssl, R4=~p", [R4]),
    R5 = erlcloud:start(),
    lager:info("init: started erlcloud, R5=~p", [R5]),
    R2 = ssh:daemon(8889, [
                      {system_dir, "/tmp/ssh_daemon"},
                      {user_dir, "/tmp/otptest_user/.ssh"},
                      {negotiation_timeout, 30}, % 30 seconds
                      {max_sessions, 2},         % maximum of 2 simultaneous connections/sessions
                      {shell, {?MODULE, no_shell, []}},
                      % {ssh_cli, no_cli},
                      {auth_methods, "publickey"},
                      % TODO: explore key_cb for using s3 as where the keys are stored
                      % {key_cb, my_key_handling_module}
                      {subsystems, [ssh_sftpd:subsystem_spec([
                                                              %{cwd, "/tmp/sftp/example"}
                                                              {file_handler, s3ftp_fserver}
                                                             ])]}
                      % failfun: a callback you can supply for when
                      % authentication fails (rate limiting? fail2ban /
                      % denyhosts?)
                      % connectfun: cb for when a user auths successfully
                      % disconnectfun: cb for when a user disconnects
                     ]),
    lager:info("init: started ssh:daemon, R2=~p", [R2]),
    % SubsystemSpec = ssh_sftpd:subsystem_spec([{cwd,"/tmp"},{root,"/tmp"}]),
    % SSHDaemonRef = ssh:daemon(22,[{subsystems,[SubsystemSpec]}]),
    {ok, { {one_for_one, 5, 10}, []} }.

