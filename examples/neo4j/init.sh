test -d $HOME/bin || mkdir $HOME/bin
if [ ! -f $HOME/bin/bake ]; then
  curl https://raw.githubusercontent.com/kyleburton/bake/master/bake > $HOME/bin/bake
  chmod 755 $HOME/bin/bake
fi
