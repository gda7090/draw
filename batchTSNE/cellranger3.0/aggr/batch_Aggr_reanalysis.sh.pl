my $vsidlist = shift;
my $csvlist = shift;
my $outdir = shift;


my @vsidlist = split/,/,$vsidlist;
my @csvlist = split/,/,$csvlist;
(-d $outdir) || (`mkdir -p $outdir`);

foreach my $vsid_index(0..$#vsidlist){
    open O, ">$vsidlist[$vsid_index]\_aggr.sh";
    print O "cd $outdir
/NJPROJ2/RNA_S/software/cellranger-3.0.0/cellranger aggr \\
\t--id=$vsidlist[$vsid_index] \\
\t--csv=$outdir/@csvlist[$vsid_index] \\
\t--normalize=mapped\n";
    print O "/NJPROJ2/RNA_S/software/cellranger-3.0.0/cellranger mat2csv $outdir/$vsidlist[$vsid_index]/outs/filtered_gene_bc_matrices_mex $outdir/$vsidlist[$vsid_index]/outs/$vsidlist[$vsid_index]\_gene_bar.csv\n";
    close O;
}




