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
    R2 = ssh:daemon(8889, [
                      {system_dir, "/tmp/ssh_daemon"},
                      {user_dir, "/tmp/otptest_user/.ssh"},
                      {shell, {?MODULE, no_shell, []}},
                      {subsystems, [ssh_sftpd:subsystem_spec([
                                                              %{cwd, "/tmp/sftp/example"}
                                                              {file_handler, s3ftp_fserver}
                                                             ])]}
                     ]),
    lager:info("init: started ssh:daemon, R2=~p", [R2]),
    % SubsystemSpec = ssh_sftpd:subsystem_spec([{cwd,"/tmp"},{root,"/tmp"}]),
    % SSHDaemonRef = ssh:daemon(22,[{subsystems,[SubsystemSpec]}]),
    {ok, { {one_for_one, 5, 10}, []} }.

