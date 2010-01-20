rm -rf mytest && ruby new-clj-mvn -v mytest
cd mytest
mvn -o assembly:assembly
