le input/differential_expression.list |perl -ne 'chommp;$_=~/.*\/(\w+)\/outs.*diffexp\/(.*)\//;print "$1\t$2\t$_"' > differential_expression.list
le input/gene_bar.list |perl -ne 'chommp;$_=~/.*\/(.*)_gene/;print "$1\t$_"' > gene_bar.list
le input/clustering.list |perl -ne 'chommp;$_=~/.*\/(\w+)\/outs.*clustering\/(.*)\//;print "$1\t$2\t$_"' > clustering.list
