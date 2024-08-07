#!/usr/bin/perl -w
#Peptides        Evalue  Conf_score      Proteins        Ksites  Qsites
use strict;

my $in = $ARGV[0];
my $mp1 = $ARGV[1];
my $mp2 = $ARGV[2];
my $out = $ARGV[3];

my %Ks = ();
my %Qs = ();

open MP1,"<$mp1";
while(<MP1>)
{
	chomp;
	$Ks{$_} = 0;
}
close MP1;

open MP2,"<$mp2";
while(<MP2>)
{
	chomp;
	$Qs{$_} = 0;
}
close MP2;

open OUT,">$out";
print OUT "Peptides\tEvalue\tConf_score\tProteins\tKsites\tQsites\ttype\n";
open IN,"<$in";
while(<IN>)
{
	chomp;
	next if(/^Peptides.+/);
	my @arr = split "\t",$_;
	print "$arr[4]\t$arr[5]\n";
	if(defined($Ks{$arr[4]}))
	{
		#print "$arr[4]\t$arr[5]\n";
		if(defined($Qs{$arr[5]}))
		{
			print OUT "$_\tcommon\n";
		}
		else
		{
			print OUT "$_\tsemi_K_common\n";
		}
	}
	elsif(defined($Qs{$arr[5]}))
	{
		print OUT "$_\tsemi_Q_common\n";
	}
	else
	{
		#print "$_\tNot_common\n";
	}	
}
close IN;
