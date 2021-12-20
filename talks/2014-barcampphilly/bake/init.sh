test -d $HOME/bin || mkdir $HOME/bin
test -f $HOME/bin/bake || curl https://raw.githubusercontent.com/kyleburton/bake/master/bake > $HOME/bin/bake
chmod 755 $HOME/bin/bake
