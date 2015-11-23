use strict;
use warnings;

sub factors {
  my($n) = @_;
  my @factors;
  for (my $ii = 2; $ii < $n; ++$ii) {
    my $rem = $n % $ii;
    if (0 == $rem) {
      push @factors, $ii;
    }
  }
  return @factors;
}

my %in_common;
for my $n (@ARGV) {
  my @factors = factors($n);
  print "$n: @factors\n";
  foreach my $ii (@factors) {
    $in_common{$ii}++;
  }
}

my $num_in_common = 0;
for my $jj (sort {$a <=> $b} keys %in_common) {
  if ($in_common{$jj} == @ARGV) {
    ++$num_in_common;
    print "common factor: $jj\n";
  }
}

if (0 == $num_in_common) {
  print "RP: @ARGV\n";
}

