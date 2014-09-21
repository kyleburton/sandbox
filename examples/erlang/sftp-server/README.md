rebar create-app appid=s3ftp

See:

* http://www.erlang.org/doc/apps/ssh/using\_ssh.html
* http://erlang.org/doc/man/ssh.html
* http://erlang.org/doc/man/ssh\_sftpd.html


To run the server:

    rebar compile && rebar shell
    1> s3ftp_app:start(1,1).

Goals:

* DONE: rebar: build the app
* DONE: configure and run an ssh server
* DONE: disable ssh / shell access
* TODO: tests for the server
* TODO: add aws library: https://github.com/gleber/erlcloud
* TODO: implement key auth using files from s3
* TODO: externalize configuration of app
* TODO: script startup / running of app
* TODO: ability to shut down the server
* TODO: ability to restart the server
* TODO: follow OTP conventions for application / server
