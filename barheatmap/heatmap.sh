python3 heatmap.py m1kmeans9clusters_Alas2filt.bcIDchange.csv M1heatmap_Alas2filtdata.order.xls M1.svg 
python3 heatmap.py s2kmeans10clusters_bcIDchange.csv S2heatmap_data.order.xls S2.svg
python3 heatmap.py s3kmeans10clusters.bcIDchage.csv S3heatmap_data.order.xls S3.svg
ls *svg |perl -ne 'chomp;$ori=$_;$_=~s/svg/png/g;`convert $ori $_`'
