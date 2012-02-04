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
wget http://code.call-cc.org/releases/4.7.0/chicken-4.7.0.tar.gz
tar xzvf chicken-4.7.0.tar.gz
cd chicken-4.7.0
