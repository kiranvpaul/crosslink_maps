#!/usr/bin/perl -w

use strict;

my $in = $ARGV[0];
my $mp = $ARGV[1];
my $out = $ARGV[2];

my %Map = ();

open MP,"<$mp";
while(<MP>)
{
	chomp;
	my($val,$freq) = split "\t",$_;
	$Map{$val} = $freq;	
}
close MP;

open OUT,">$out";
open IN,"<$in";
while(<IN>)
{       
        chomp;
	if(/^Peptide.+/)
	{
		print OUT "$_\tFreq\n";
	}
	else
	{
        	my @arr = split "\t",$_;
		my $pair = "$arr[4]-$arr[5]";
 		if(defined($Map{$pair}))
		{
			print OUT "$_\t".$Map{$arr[4]."-".$arr[5]}."\n";
		}
	}       
}
close IN;
close OUT;
