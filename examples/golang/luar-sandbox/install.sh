set -exu
PKGS="liblua5.1-0-dev"
sudo apt-get install "$PKGS"
go get github.com/stevedonovan/luar
