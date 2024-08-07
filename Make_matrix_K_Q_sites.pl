#!/usr/bin/perl -w

use strict;

my $in = $ARGV[0];
my $out = $ARGV[1];
my %Cancer = ();
my %genes = ();

open IN,"<$in" or die "cannot open file for reading\n";
while(<IN>)
{
	chomp;
	next if(/^Crosslink.+/);
	my($val,$freq) = split "\t",$_;
	my($k,$q) = split "-",$val;
	my @K = split "_",$k;
	my @Q = split "_",$q;
	my $ksite = $K[0]."_".$K[2];
	my $qsite = $Q[0]."_".$Q[2];
	#my($can,$Ge,$freq) = split "\t",$_;
	$Cancer{$ksite}{$freq} = 0;
	$genes{$qsite}{$ksite}{$freq} = 0;	
}
close IN;

open OUT,">$out" or die "cannot open file for writing\n";
my $head = join "\t",('Qsites', (sort{$a cmp $b;} keys %Cancer));
print OUT "$head\n";

foreach my $Gene(sort{$a cmp $b;} keys %genes)
{
	my @Freq = ();
	foreach my $can(sort{$a cmp $b;} keys %Cancer)
	{
		#foreach my $freq(keys %{$genes{$Gene}{$can}})
		#{
		my $freq = 0;
		if(defined($genes{$Gene}{$can}))
		{
			foreach $freq(keys %{$genes{$Gene}{$can}})
			{
				push(@Freq,$freq)
			}
		}
		else
		{
		#print "$Gene\t$can\t$freq\n";
			push(@Freq,$freq);
		}
	}
	my $Matrix = join "\t",@Freq;
	print OUT "$Gene\t$Matrix\n";

}
close OUT
