#!/usr/bin/perl -w
#sp      E9PV24-2        FIBA_MOUSE (395)-sp     E9PV24-2        FIBA_MOUSE (241)
use strict;

my $in = $ARGV[0];
my $Mat = $ARGV[1];

my %mat = ();
my %Cancer = ();
my %genes = ();

open MT, ">$Mat";
#print OUT "Peptides\tEvalue\tConf_score\tProteins\tKsites\tQsites\n";
open IN,"<$in";
while(<IN>)
{
	chomp;
	next if(/^Peptides.+/);
	my @arr = split "\t",$_;
	my $conf_score = log10($arr[2]);
	my($k1,$k2,$k3) = split "_",$arr[4];
        my($q1,$q2,$q3) = split "_",$arr[5];
        my $v1 = $k1."_".$k3;
        my $v2 = $q1."_".$q3;
        $mat{$v1."\t".$v2}{$conf_score} = 0;
}
close IN;

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
close MT;
