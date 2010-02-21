export CLASSPATH="target/test-classes:target/classes:`mvn -Dmdep.outputFile=/dev/stderr dependency:build-classpath 2>&1 > /dev/null`"

jirb "$@"
