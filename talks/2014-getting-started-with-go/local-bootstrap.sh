set -eu
sudo apt-get update
sudo apt-get install -y maven2 openjdk-7-jdk openjdk-7-doc emacs24 emacs24-el vim vim-doc vim-scripts git mercurial subversion rake
test -d $HOME/bin/ || mkdir $HOME/bin/

if [ ! -x $HOME/bin/lein ]; then
  curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > $HOME/bin/lein
  chmod 755 $HOME/bin/lein
  $HOME/bin/lein
fi

cd
if [ ! -d .profile.d ]; then
  git clone https://github.com/kyleburton/profile.git .profile.d
  mv .bashrc .dist.bashrc
  cd $HOME/.profile.d && bash install.sh kburton
fi

test -d $HOME/projects || mkdir $HOME/projects
cd $HOME/projects
test -d krbemacs || git clone https://github.com/kyleburton/krbemacs.git
cd krbemacs
test -f $HOME/.emacs || ln -s $(pwd)/.emacs $HOME/.emacs
cd
#sudo gem install base_app

test -d $HOME/.lein || mkdir $HOME/.lein
cp droplet-init/files/self/.lein/profiles.clj $HOME/.lein/

cd $HOME/projects
if [ ! -d dev-utils ]; then
  git clone https://github.com/kyleburton/dev-utils.git
fi

cd dev-utils/instago
if which go; then
  echo "go already installed"
else
  rake install
fi

set +u
. ~/.bashrc

go get github.com/kyleburton/diocean
go get github.com/kyleburton/go-abtab/cmd/abtab
cd $GOPATH/src/github.com/kyleburton/go-abtab && bash install.sh
