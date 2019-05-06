#software/miniconda3/lib/R/bin/Rscript annoheatmap.R
args <- commandArgs(T)
if(length(args)<2){
    cat ("Example:  software/miniconda3/lib/R/bin/Rscript annoheatmap.R test.csv hlist.svg\n")
	    quit("no")
		}

library(circlize) #for heatmap colors
library(ComplexHeatmap)
library(RColorBrewer) # for group colors 
library(tidyverse)


ko<-read_csv(args[1],col_names = T,  
  col_types = cols(id = col_character()))
ko[rowSums(ko[,7:ncol(ko)])!=0,]->ko
column_to_rownames(ko,var = 'X1_1') -> ko1
k<-as.data.frame (ko1)

cells<-colnames(k)[6:ncol(k)]
str_split(cells, "\\.",n=2,  simplify = TRUE)->a 
data.frame(sample=a[,2])->ha_c
#rownames(ha_c)<-cells

k[-c(1,2,3,4,5)]-> hmp 
glen=k$end -k$start
celwidth=glen/sum(glen)

hmp[hmp==0]<-NA


df<- t(log10(hmp))
col_fun = colorRamp2(c(min(df[!is.na(df)]),max(df[!is.na(df)])), c("pink","darkred"))
cl<-c("1","2","3")
names(cl)<-c("red","blue","green")
h1a = rowAnnotation(ha_c,
                    col = list(sample=c("1" =  "red", "2" = "blue","3"="green")),
                    width = unit(0.2, "cm"),
					annotation_legend_param = list(
					    at=c("1","2","3"),
					    labels = c("m1", "s2", "s3")
						))

h1<-Heatmap(matrix = df,name = "log10",
        col = col_fun,
        na_col = "white",
        cluster_rows = F,
        show_row_names = F,
        show_column_names = F,
        cluster_columns=F,

        cell_fun = function(j, i, x, y, w, h, fill){
          w=celwidth[j]
          h=h
          x<-ifelse(j==1, celwidth[j]/2, sum(celwidth[1:j-1])+celwidth[j]/2)
          grid.rect(x = x, y = y, width = w, height = h, 
                    gp = gpar(col = NA, fill = fill))}
)

lineidx<-c()
xtxt <- c()
j=1                                                    
for (i in unique(k$id)){
  lineidx[j]<-max(which(k$id==i))
  xtxt[j]<-sum(celwidth[1:lineidx[j]])
  j=j+1}

hlist<-h1 + h1a 

svg(file=args[2])
draw(hlist, row_title = NA, column_title = "  ",column_title_side="bottom")

decorate_heatmap_body("log10", {
    ytxt = rep(-0.02,length(lineidx))   
    x = rep(xtxt,2)
    y = rep(c(0,1),each=length(lineidx))
    grid.polyline(x, y, 
        id=rep(seq(length(lineidx)),2),
        gp = gpar(lwd = 0.5, lty = 2))
    grid.text(
	    k$id[lineidx], x=xtxt, y=ytxt,
        gp = gpar(fontsize=8)
	)
})

dev.off()


