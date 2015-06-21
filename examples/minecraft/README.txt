Install bake:

    https://github.com/kyleburton/bake
    # or
    test -d $HOME/bin || mkdir $HOME/bin
    curl https://raw.githubusercontent.com/kyleburton/bake/master/bake > $HOME/bin/bake
    chmod 755 $HOME/bin/bake

Download / Install / Build the necessaries:

    bake init

Fail to Start the canarymod sever:

    bake run_canarymod_server

Accept the EULA:

    vim eula.txt

Run the canarymod server:

    bake run_canarymod_server

When that comes up, start cider:

   /cider

In a differetn termainl, run Emacs and connect to the running server:

    bake run_emacs src/krb/scratch.clj
    # or
    emacs -q -nw -l .emacs

Then connect to the server:

    C-c s s 
    ;; or
    M-x cider-connect

References

* Learn to Program with Minecraft Plugins - Andy Hunt
