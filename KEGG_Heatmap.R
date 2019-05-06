################################################################
# Plot the KEGG terms heatmap
# Input file :
# 1.KEGG_classification_count.txt (each compare)
# 2.CAD_GO_enrichment_result.xls (each compare)
# 3.Diff_genes.FPKM.rowmeans.xls (each compare)
###############################################################
# by ncRNA Group   2014-09-25
###############################################################
# version 2.3
#limited description length (<= 30 characters)
#limited term number (<= 30)
##############################################################


args<-commandArgs(TRUE)
kegg_path<-args[1]
diff_gene_exp<-args[2]
compares_name<-args[3]
output_dir<-args[4]
otype<-args[5]
id2tr<-args[6]

library("pheatmap")
library("reshape2")
setwd(output_dir)
#--------------   color prepare   --------------------------
ColorMatrix = c("#a50026","#d73027","#f46d43","#fdae61","#fee08b","#ffffbf","#d9ef8b","#a6d96a","#66bd63","#1a9850","#006837")
ColorMatrix = ColorMatrix[seq(11,1,-1)]

#--------------  KEGG_classify data --------------------------
total_KEGG_classify = data.frame(V1=1)
compare_files = unlist(strsplit(compares_name,","))
for (each_compare in compare_files)
{
  in_file_sig = paste(kegg_path,"/",each_compare,"/Pathway/","top_20.add.",each_compare,".identify.xls",sep = "")
  if(file.info(in_file_sig)$size>50){
    selected = read.table(in_file_sig,skip = 1,header = F,sep = "\t")
    selected = data.frame(V1=selected$V1)
    total_KEGG_classify = rbind(total_KEGG_classify,selected)
  }
}

total_KEGG_classify = total_KEGG_classify[seq(2,(dim(total_KEGG_classify)[1])),]

unique_KEGG = unique(total_KEGG_classify)
KEGG_term_name = unique_KEGG


#---------------- get gene id in each GO_term -----------------------------
Number_terms = length(unique_KEGG)
Gene_Array = as.character(seq(1,Number_terms)*0)
for (each_compare in compare_files){
  in_file_gene = paste(kegg_path,"/",each_compare,"/Pathway/","add.",each_compare,".identify.xls",sep = "")
  if(file.info(in_file_gene)$size>50){
  get_gene = read.delim(in_file_gene,header = T,sep = "\t",skip = 4)
  get_gene = get_gene[,c("X.Term","Input")]
  gene_index = match(unique_KEGG,get_gene$X.Term)
  TempGenes = get_gene[gene_index,2]
  for (id_a in seq(1,Number_terms)){
    if (!is.na(TempGenes[id_a])){
      Gene_Array[id_a] = paste(Gene_Array[id_a],TempGenes[id_a],sep = "|")
    }
  }
  }
}

total_KEGG_gene = data.frame(GO_ID = unique_KEGG, Gene_ID =Gene_Array)

#--------------  Expression data --------------------------
Diff_gene_expr = read.table(diff_gene_exp,header = T)
number_sample = dim(Diff_gene_expr)[2]

KEGG_Expr = Diff_gene_expr[1,seq(2,number_sample)]
KEGG_Expr[1,] = 0
## pdf(paste(otype,"_each_KEGG_term_heatmap.pdf",sep=""),onefile=FALSE)
for (id_b in seq(1,Number_terms))
{
  unique(strsplit(as.character(total_KEGG_gene[id_b,2]),"|")[[1]])
  each_term_genes = unique(strsplit(as.character(total_KEGG_gene[id_b,2]),"[|]")[[1]])
  Index_gene = match(each_term_genes,as.character(Diff_gene_expr$gene_id))
  each_term_expr = Diff_gene_expr[na.omit(Index_gene),seq(2,number_sample)]
  KEGG_term_name[id_b] = paste(KEGG_term_name[id_b],"(",dim(each_term_expr)[1],")")

  #---------- each term each figure
  Temp_each_term = each_term_expr
  rownames(Temp_each_term) <- Diff_gene_expr[na.omit(Index_gene),1]
  Temp_each_term = log2(Temp_each_term+1)
  num_gene = dim(Temp_each_term)[1]
  if (num_gene > 1 && num_gene < 30)
  {
    ## pheatmap(as.matrix(Temp_each_term),col = ColorMatrix,cluster_cols = T,main =KEGG_term_name[id_b],scale = "column",show_rownames = T)
  }
  if (num_gene > 30)
  {
    ## pheatmap(as.matrix(Temp_each_term),col = ColorMatrix,cluster_cols = T,main =KEGG_term_name[id_b],scale = "column",show_rownames = F)
  }
  KEGG_Expr_temp = apply(each_term_expr,2,mean)
  KEGG_Expr = rbind(KEGG_Expr,KEGG_Expr_temp)
}
## dev.off()

KEGG_Expr = KEGG_Expr[seq(2,(Number_terms+1)),]
rownames(KEGG_Expr) <- KEGG_term_name
KEGG_Expr = na.exclude(KEGG_Expr[seq(1,(Number_terms+1)),])
KEGG_Expr = log2(KEGG_Expr+1)
KEGG_Expr = head(KEGG_Expr,30)

write.table(KEGG_Expr, "after-sale/NHT150112-as/kegg.xls")
## #----------------  out figure     ----------------------------------------
## num_gene=dim(KEGG_Expr)[1]
## if(num_gene > 30){
## 	pdf(paste(otype,"_KEGG_term_heatmap.pdf",sep=""),width = 10,height = 10,onefile=FALSE)
## }else{
## 	pdf(paste(otype,"_KEGG_term_heatmap.pdf",sep=""),width = 8,height = 8,onefile=FALSE)
## }
## pheatmap(KEGG_Expr,col = ColorMatrix,cluster_cols = T,scale = "column")
## dev.off()

## if(num_gene > 30){
## 	png(paste(otype,"_KEGG_term_heatmap.png",sep=""),type="cairo-png",width = 750, height = 750)
## }else{
## 	png(paste(otype,"_KEGG_term_heatmap.png",sep=""),type="cairo-png")
## }
## pheatmap(KEGG_Expr,col = ColorMatrix,cluster_cols = T,scale = "column")
## dev.off()
