sudo apt-get update
sudo apt-get upgrade

sudo apt-get install ruby1.9.1 git emacs23-nox wget curl

cd
if [ ! -d .profile.d ]; then
  git clone git@github.com:kyleburton/profile.git .profile.d
  mv .bashrc .dist.bashrc
  cd .profile.d
  bash install.sh kburton
  cd
fi

cd
if [ ! -d projects ]; then
  test -d projects || mkdir projects
fi


cd projects
GITPROJS="sandbox teporingo clj-etl-utils system-utils krbemacs jrclj clj-bloom clj-xpath credit_card_validator impresario lein-marginalia lein-margauto perCEPtor dev-utils twilio-in-ten-minutes tellmewhen abstract-tables large-data-and-clojure base-app jbit clorine abottleinfrontofme kyleburton clj-lfsr fuzzy-string typrtail kyles-secret-interviewing-techniques intro-to-genetic-algorithms introduction-to-git"

for proj in $GITPROJS; do
  git clone git@github.com:kyleburton/$proj.git
done

mkdir ~/projects/sandbox/dev-setup/linux/software
cd ~/projects/sandbox/dev-setup/linux/software
CHICKEN_VER=4.7.0

download_file() {
  F=$1
  URL=$2
  if [ ! -f $F ]; then
    wget $URL
  fi
}

untar_file() {
  D=$1
  F=$2
  test -d $D || tar xzvf $F

}

download_file chicken-$CHICKEN_VER.tar.gz http://code.call-cc.org/releases/$CHICKEN_VER/chicken-$CHICKEN_VER.tar.gz
untar_file chicken-$CHICKEN_VER chicken-$CHICKEN_VER.tar.gz
cd chicken-$CHICKEN_VER
make PLATFORM=linux
make PLATFORM=linux PREFIX=$HOME/local/chicken-$CHICKEN_VER install
ln -s $HOME/local/chicken-$CHICKEN_VER $HOME/local/chicken
