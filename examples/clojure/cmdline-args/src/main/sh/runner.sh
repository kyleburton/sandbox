
JAR_WITH_DEP=@target.directory@/@maven.test.classpath@
CLASSPATH="$JAR_WITH_DEP"
    #-Xdebug \
    #-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8886 \

java -server \
    -cp "$CLASSPATH" \
    com.github.kyleburton.cmdline_example.main \
    "@target.directory@/classes/swank_runner.clj" \
    "$@"
