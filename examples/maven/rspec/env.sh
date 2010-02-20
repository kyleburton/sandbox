if [ -z "$JRUBY_HOME" ]; then
  if which jruby 2>&1 > /dev/null; then
    export JRUBY_HOME="$(dirname $(dirname $(which jruby)))"
    echo "Found JRUBY_HOME=$JRUBY_HOME"
  else
    echo "Unable to determine JRUBY_HOME, you must set JRUBY_HOME to point to your JRUBY installation"
  fi
else
    echo "Found JRUBY_HOME=$JRUBY_HOME"
fi
