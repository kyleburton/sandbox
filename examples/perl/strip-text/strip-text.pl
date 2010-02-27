use strict;
use warnings;

while(<>) {
  chomp;
  # strip the closing tag 'example' from lines, when it is at the end, NOT allowing for trailing spaces
  #s|</example>$||i;
  # strip the closing tag 'example' from lines, when it is at the end, allowing for trailing spaces
  #s|</example>\s*$||i;
  # strip the last closing XML tags on a line, allowing for trailing spaces, even if it's the only thing on the line
  #s|</[^>]+>\s*$||i;
  # strip the last closing XML tags on the line, allowing for trailing spaces, but only if there is some text before the closing tag
  s|(?<=.)</[^>]+>\s*$||i;
  print $_,"\n"
}
