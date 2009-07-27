set -e
set -x

#  test -d clojure || git clone git://github.com/richhickey/clojure.git
#  cd clojure
#  ant jar
#  mvn install:install-file \
#    -Dfile=clojure-1.1.0-alpha-SNAPSHOT.jar \
#    -DgroupId=org.clojure \
#    -DartifactId=clojure \
#    -Dversion=1.1.0-alpha-SNAPSHOT \
#    -Dpackaging=jar
#  cd ../
#  
#  test -d clojure-contrib || git clone git://github.com/richhickey/clojure-contrib.git
#  cd clojure-contrib
#  ant jar
#  mvn install:install-file \
#    -Dfile=clojure-contrib.jar \
#    -DgroupId=org.clojure \
#    -DartifactId=clojure-contrib \
#    -Dversion=1.1.0-alpha-SNAPSHOT \
#    -Dpackaging=jar
#  cd ..

test -f rabbitmq-java-client-1.6.0.tar.gz || wget http://www.rabbitmq.com/releases/rabbitmq-java-client/v1.6.0/rabbitmq-java-client-1.6.0.tar.gz
test -d tar xzvf rabbitmq-java-client-1.6.0 || tar xzvf rabbitmq-java-client-1.6.0.tar.gz
cd rabbitmq-java-client-1.6.0
ant dist
mvn install:install-file \
  -Dfile=build/lib/rabbitmq-client.jar \
  -DgroupId=com.rabbitmq \
  -DartifactId=amqp-client \
  -Dversion=1.6.1-SNAPSHOT \
  -Dpackaging=jar
cd ..
