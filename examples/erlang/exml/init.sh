set -exu

test -d software || mkdir software

cd software
if [ ! -f kerl ]; then
  curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
  chmod 755 kerl
fi

if [ ! -d rebar ]; then
  git clone git://github.com/rebar/rebar.git
fi

cd rebar
if [ ! -x rebar ]; then
  ./bootstrap
fi

cd ../../
