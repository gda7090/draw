#grep "Sry" /NJPROJ2/RNA_S/personal_dir/lindan/SHOUHOU/P101SC18080982-01-B1/Aggr/EvsLvsAfvsoldvsPG/EvsLvsAf_1vsAf_2vsAf_3vsAf_4vsold_1vsold_2vsold_3vsPG_1vsPG_2/outs/filtered_gene_bc_matrices_mex/mm10/genes.tsv
##ENSMUSG00000069036  Sry
perl gene_bar.filt.pl CellRanger/E/E/outs/E_gene_bar.csv >E_gene_bar.filt.csv 
qsub -S /NJPROJ1/RNA/software/R/R-3.4.2/bin/Rscript -V -l vf=2g,p=2 -cwd sry.cellparted.R
perl batch_Aggr_reanalysis.sh.pl EvsLvsAmvsOld EvsLvsAmvsOld.lib.csv /NJPROJ2/RNA_S/personal_dir/lindan/SHOUHOU/P101SC18080982-01-B1/20181206_SRY_Repeatability_Aggr/SRY
qsub -V -cwd -l vf=50G,p=10 -P smp512 EvsLvsAmvsOld_aggr.sh
perl batch_Aggr_reanalysis.sh.pl EvsAf EvsAf.lib.csv /NJPROJ2/RNA_S/personal_dir/lindan/SHOUHOU/P101SC18080982-01-B1/20181206_SRY_Repeatability_Aggr/SRY
qsub -V -cwd -l vf=50G,p=10 -P smp512 EvsAf_aggr.sh
perl batch_Aggr_reanalysis.sh.pl PgvsAf PgvsAf.lib.csv /NJPROJ2/RNA_S/personal_dir/lindan/SHOUHOU/P101SC18080982-01-B1/20181206_SRY_Repeatability_Aggr/SRY
qsub -V -cwd -l vf=50G,p=10 -P smp512 PgvsAf_aggr.sh

