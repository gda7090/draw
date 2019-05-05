
args<-commandArgs(T)
if(length(args)<3){
  cat ("Example: InsertNonClusterMeans.R clusters.csv.list differential_expression.csv.list mB_gene_bar.csv.list outputdir \n")
  quit("no")
}

suppressPackageStartupMessages(library("tidyverse"))

mkdir <- function(outdir){
  if(!file.exists(outdir)){dir.create(outdir,recursive = T)}
}

InsertNonClusterMeans <- function(
  cluster, #cluster tibble
  dif,     #differential_expression tibble
  gbar,    # gene_bar tibble
  outdir   # outputdir
){
  gbar[(gbar[[1]] %in% dif[[1]]),] %>% #select diff genes
    column_to_rownames(var="X1") %>%
    t() %>%  #matrix
    as.data.frame() %>%
    rownames_to_column(var='Barcode') %>%
    left_join(cluster, by = "Barcode")-> joined_gbar #barcode\tabundance...\tcluster\n
  
  group_by(joined_gbar[,-1],Cluster)->gb 
  count(gb)->cgb # each_cluster_size
  colSums(joined_gbar[,c(-1,-ncol(joined_gbar))]) ->sum_all #sum all the clusters
  as.data.frame(summarise_all(gb,funs(sum))) ->sum_each
  t(t((sum_all-t(sum_each[,-1])))/(nrow(joined_gbar)-cgb$n)) -> non_cluster_mean #non_cluster_mean matrix
  
   # 矩阵乘法用%*%
  # 矩阵除法就是求逆矩阵，需要是方阵。
  
  colnames(dif)[str_detect( colnames(dif),"Mean")] %>%
    str_replace_all("Cluster","Non Cluster") -> colnames(non_cluster_mean)
  
  as.data.frame(non_cluster_mean,row.names=row.names(non_cluster_mean)) %>%
    rownames_to_column(var="Gene ID") -> df  
  # NondifF <- paste(outdir,"differential_expression.csv",sep="/")
  # write_csv(df, NondifF)
  
  #insert to ori diff csv
  for (name in colnames(df)[-1]){
    str_replace(name,"Non ","") -> oldname
    which(oldname == colnames(dif))->coln
    dif <- add_column(dif, df[[name]], .after = coln)
    colnames(dif)[coln+1] <- name
  }
  
#  as.data.frame(non_cluster_mean,row.names=row.names(non_cluster_mean)) %>%
#    rownames_to_column(var="Gene ID") -> df 
  InsertdifF <- paste(outdir,"differential_expression.csv",sep="/")
  write_csv(dif, InsertdifF)
}

## main ##
# clusters.csv.list differential_expression.csv.list mB_gene_bar.csv.list 
clusters <- read.table(args[1])
difs <- read.table(args[2])
gbars <- read.table(args[3])
outputdir <- args[4]

n=nrow(gbars)
for (i in 1:n){
  smpl <- as.character(gbars[,1][i])
  gbar<-read_csv(as.character(gbars[i,2]))
  cluster<-clusters[as.character(clusters[,1])==smpl,]
  dif <- difs[as.character(difs[,1])==smpl,]
  
  difn <- nrow(dif)
  for (j in 1:difn){
    clst <- as.character(dif[,2][j])
    difj <- read_csv(as.character(dif[j,3]))
    clstf <- cluster[as.character(cluster[,2])==clst, 3]
    clusterj <-read_csv(as.character(clstf))
    outdir<-paste(outputdir,smpl,clst,sep="/")
    mkdir(outdir)
    InsertNonClusterMeans(clusterj,difj,gbar,outdir)
  }
}
