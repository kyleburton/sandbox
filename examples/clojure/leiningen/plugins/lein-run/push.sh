set -e 
set -x
lein deps
lein jar
lein pom
scp pom.xml lein-run.jar clojars@clojars.org:
