test -f /tmp/foobar.dat && rm /tmp/foobar.dat 

gcc -Wall fork.c  && ./a.out

echo ""
echo "Contents of /tmp/foobar.dat: "
cat /tmp/foobar.dat
