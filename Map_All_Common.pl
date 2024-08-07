#!/usr/bin/perl -w

use strict;

my $in1 = $ARGV[0];
my $in2 = $ARGV[1];
my $mp = $ARGV[2];
my $out = $ARGV[3];

my %site = ();
my %uniq = ();
open MP,"<$mp";
while(<MP>)
{
	chomp;
	next if(/^Peptides.+/);
	my @arr = split "\t",$_;
	#if($arr[6] eq "C")
	#{	
		$site{$arr[4]."\t".$arr[5]} = "$arr[1]\t$arr[2]\t$arr[7]";	
	#}
}
close MP;

open OUT,">$out";
open IN1,"<$in1";
print OUT "Ksites\tQsites\tEvalue\tConf_score\tFreq\n";
while(<IN1>)
{
	chomp;
	my $k = $_;
	open IN2,"<$in2";
	while(<IN2>)
	{
		chomp;
		my $q = $_;
		if(defined($site{$k."\t".$q}))
		{
			print OUT "$k\t$q\t".$site{$k."\t".$q}."\n";
		}
		else
		{
			print OUT "$k\t$q\t0\t0\t0\n";
		}
	}
	close IN2;	
}
close IN1;
close OUT;
