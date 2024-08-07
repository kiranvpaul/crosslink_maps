#!/usr/bin/perl -w
#sp      E9PV24-2        FIBA_MOUSE (395)-sp     E9PV24-2        FIBA_MOUSE (241)
use strict;

my $in = $ARGV[0];
my $out = $ARGV[1];
my $Mat = $ARGV[2];
my $eval = $ARGV[3];

my %uniq = ();

open OUT, ">$out";
open MT, ">$Mat";
print OUT "Peptides\tEvalue\tConf_score\tProteins\tKsites\tQsites\n";
open IN,"<$in";
while(<IN>)
{
	chomp;
	next if(/^Order.+/);
	my @arr = split "\,",$_;
	#print "$arr[4]\t$arr[9]\t$arr[9]\t$arr[13]\n";
	my @arr1 = split '/',$arr[13];
	my @val = split '\|',$arr1[0];
	#print "$val[0]\t$val[1]\t$val[2]\t$val[3]\t$val[4]\n";
	my $prot1 = '';
	my $pos1 = '';
	my $prot2 = '';
       	my $pos2 = '';
	my $aa1 = '';
	my $aa2 = '';
	if($val[2] =~ /(\S+)\s+\((\d+)\).+/)
	{
		$prot1 = $1;
		$pos1 = $2;
	}
	if($val[4] =~ /(\S+)\s+\((\d+)\).*/)
	{
		$prot2 = $1;
		$pos2 = $2;
	}
	if($arr[4] =~ /^(\S+)\((\d+)\)\-(\S+)\((\d+)\)/)
     	{
		$aa1 = substr($1,$2-1,1);
		$aa2 = substr($3,$4-1,1);
		#print "$1\t$2\t$aa1\t$3\t$4\t$aa2\n";
	}
	if($arr[9] < $eval)
	{
		if($aa1 eq "K")
		{
			#print OUT "$arr[4]\t$arr[9]\t$arr1[$i]\t$prot1"._."$pos1\t$prot2"._."$pos2\n";
			my $Prot1 = "$prot1"._."$pos1";
			my $Prot2 = "$prot2"._."$pos2";
			$uniq{$arr[4]."\t".$Prot1."\t".$Prot2."\t".$arr1[0]}{$arr[9]} = 0;
		}
		else
		{
			#print OUT "$arr[4]\t$arr[9]\t$arr[13]\t$prot2"._."$pos2\t$prot1"._."$pos1\n";
			my $Prot1 = "$prot2"._."$pos2";
			my $Prot2 = "$prot1"._."$pos1";
			$uniq{$arr[4]."\t".$Prot1."\t".$Prot2."\t".$arr1[0]}{$arr[9]} = 0;
		}
	}
}
close IN;

my %mat = ();
my %Cancer = ();
my %genes = ();

foreach my $comb(keys %uniq)
{
	my @spec = split "\t",$comb;
	my @eval = ();
	foreach my $evl(keys %{$uniq{$comb}})
	{
		push(@eval,$evl);
	}
	my @sortedEval = sort {$a <=> $b;} @eval;
	my $joinEval = join ",",@eval;
	my $conf_score = log10(1/$sortedEval[0]);
	print OUT "$spec[0]\t$sortedEval[0]\t$conf_score\t$spec[3]\t$spec[1]\t$spec[2]\n";
	my($k1,$k2,$k3) = split "_",$spec[1];
	my($q1,$q2,$q3) = split "_",$spec[2];
	my $v1 = $k1."_".$k3;
	my $v2 = $q1."_".$q3; 
	$mat{$v1."\t".$v2}{$conf_score} = 0;
}
sub log10
{
	my $n = shift;
	return log($n)/log(10);
}

foreach my $val(keys %mat)
{
	my($v1,$v2) = split "\t",$val;
	my @ev = ();
	foreach my $e(sort{$b <=> $a;} keys %{$mat{$val}})
	{
		push(@ev,$e);
	}
	my @sortedEval = sort {$b <=> $a;} @ev;
	#print "$val\t$sortedEval[0]\n";
	$Cancer{$v1}{$sortedEval[0]} = 0;
	$genes{$v2}{$v1}{$sortedEval[0]} = 0;
}

my $head = join "\t",('Qsites', (sort{$a cmp $b;} keys %Cancer));
print MT "$head\n";

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
	print MT "$Gene\t$Matrix\n";

}
close OUT;
close MT;
