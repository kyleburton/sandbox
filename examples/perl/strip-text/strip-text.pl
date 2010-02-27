use strict;
use warnings;

while(<>) {
  chomp;
  s|</example>\s*$||;
  print $_,"\n"
}
