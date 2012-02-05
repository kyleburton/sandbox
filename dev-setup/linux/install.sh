. common.sh
sudo apt-get -y update
sudo apt-get -y upgrade

sudo apt-get -y install ruby1.9.1 git emacs23-nox wget curl rake

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
GITPROJS="sandbox teporingo clj-etl-utils system-utils krbemacs jrclj clj-bloom clj-xpath credit_card_validator impresario lein-marginalia lein-margauto perCEPtor dev-utils twilio-in-ten-minutes tellmewhen abstract-tables large-data-and-clojure base-app jbit clorine abottleinfrontofme clj-lfsr fuzzy-string typrtail kyles-secret-interviewing-techniques intro-to-genetic-algorithms introduction-to-git"

for proj in $GITPROJS; do
    test -d $proj || git clone git@github.com:kyleburton/$proj.git
done

test -d ~/projects/sandbox/dev-setup/linux/software || mkdir ~/projects/sandbox/dev-setup/linux/software
cd ~/projects/sandbox/dev-setup/linux/software
CHICKEN_VER=4.7.0

if [ ! -x $HOME/local/chicken/bin/csi ]; then
    download_file chicken-$CHICKEN_VER.tar.gz http://code.call-cc.org/releases/$CHICKEN_VER/chicken-$CHICKEN_VER.tar.gz
    untar_file chicken-$CHICKEN_VER chicken-$CHICKEN_VER.tar.gz
    cd chicken-$CHICKEN_VER
    make PLATFORM=linux
    make PLATFORM=linux PREFIX=$HOME/local/chicken-$CHICKEN_VER install
    ln -s $HOME/local/chicken-$CHICKEN_VER $HOME/local/chicken
fi


# rabbitmq
if [ ! -f /etc/apt/sources.list.d/rabbitmq-sources.list ]; then
    echo "deb http://www.rabbitmq.com/debian/ testing main" > rabbitmq-sources.list
    sudo cp rabbitmq-sources.list /etc/apt/sources.list.d/
    wget http://www.rabbitmq.com/rabbitmq-signing-key-public.asc
    sudo apt-key add rabbitmq-signing-key-public.asc
    sudo apt-get update
fi

sudo apt-get -y install rabbitmq-server

if [ ! -x $HOME/bin/lein ]; then
    cd
    test -d bin || mkdir bin
    cd bin
    wget https://raw.github.com/technomancy/leiningen/stable/bin/lein
    chmod 755 lein
    ./lein help
fi
