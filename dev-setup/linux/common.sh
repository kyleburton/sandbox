download_file() {
    F=$1
    URL=$2
    if [ ! -f $F ]; then
        wget $URL
    fi
}

download() {
    URL=$1
    F=$(basename $1)
    if [ ! -f $F ]; then
        wget $URL
    fi
}

untar_file() {
    D=$1
    F=$2
    test -d $D || tar xzvf $F

}


ensure_dir () {
  test -d "$1" || mkdir "$1"
}

unzip_unless() {
  F=$1
  Z=$2
  test -e $F || unzip $Z
}
