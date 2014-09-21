Use Erlang's SFTP daemon with AWS S3 as a backing store.

My use-case is:

  * a client drops a file off on our sftp server 
  * we want to auto-process the file, which involves:
    * seeing that it arrived
    * placing it in shared storage so other systmes can access it (s3)

See:

* http://www.erlang.org/doc/apps/ssh/using\_ssh.html
* http://erlang.org/doc/man/ssh.html
* http://erlang.org/doc/man/ssh\_sftpd.html

    rebar create-app appid=s3ftp

To run the server:

    rebar compile && rebar shell
    1> s3ftp_app:start(1,1).

Goals:

* DONE: rebar: build the app
* DONE: configure and run an ssh server
* DONE: disable ssh / shell access via function that refuses
* TODO: disable ssh / shell access via {ssh\_cli, no\_cli}
* DONE: add aws library: https://github.com/gleber/erlcloud
* DONE: basic list objects in s3 bucket
* TODO: Support changing directory
* TODO: Support removing a file
* TODO: Support creating a directory
* TODO: Support put [uploading] a file
* TODO: Support get [downloading] a file
* TODO: implement key auth using files from the s3 bucket
* TODO: externalize configuration of app: aw3 api credentials, key prefix
* TODO: script startup / running of app
* TODO: ability to shut down the server
* TODO: ability to restart the server
* TODO: follow OTP conventions for application / server / supervision
* TODO: tests
* TODO: monitoring / reporting / metrics hooks
* TODO: retry on s3 failure
