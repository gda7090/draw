# /TJPROJ1/MICRO/yangfenglong/software/conda/miniconda3/bin/python3 heatmap.py kmeans9clusters_Alas2filt.csv  M1heatmap_Alas2filtdata.xls S1.heatmap.svg
from matplotlib import ticker, pyplot as plt
import numpy as np
import matplotlib.patches as mpatches
import matplotlib as mpl
import pandas as pd #read tables
import sys
plt.switch_backend('agg') # does not plot to screen for RuntimeError: Invalid DISPLAY variable

# readin and sort tables
anno=pd.read_csv(sys.argv[1],sep=None,header=0,engine='python')
anno=anno.sort_values(by=[anno.columns.values[1],anno.columns.values[0]])
anno.set_index(anno.columns.values[0],inplace=True)
# anno.iloc[:,0]
table=pd.read_table(sys.argv[2],sep=None,header=0,index_col=0,engine='python')
table=table[anno.index]

#print out the orderd tables
anno.to_csv("anno.xls",sep="\t")
table.to_csv("table.xls",sep="\t")
# color=[mpl.colors.rgb2hex(col[i-1]) for i in anno.rank]
rank={}
j=1
for i in anno.iloc[:,0].values:
    if i in rank:
        next
    else:
        rank[i]=j
        j +=1
anno.rank = [rank[i] for i in anno.iloc[:,0].values]
#for top bar colors
# cmap=mpl.cm.Vega10
colors = ['red', 'green', 'orange', 'blue', 'sandybrown', 'purple','pink','olive','cyan']
bounds = [1,2,3,4,5,6,7,8,9]
# c=[colors[i-1] for i in anno.rank]
cmap = mpl.colors.ListedColormap(colors)
# c=[colors[i-1] for i in [1,1,1,1,2,2]]
# cmap = mpl.colors.ListedColormap(c)
# cmap = mpl.colors.ListedColormap(colors)
# c = [colors[i-1] for i in anno.rank]
# norm = mpl.colors.BoundaryNorm(bounds, cmap.N)

label=[np.unique(anno.iloc[:,0])[i-1] for i in np.unique(anno.rank)]
# col = [mpl.colors.rgb2hex(cmap.colors[i-1])  for i in np.unique(anno.rank)]
col = [colors[i-1]  for i in np.unique(anno.rank)]
# bounds = np.unique(group.Cluster)
# cmap = mpl.colors.ListedColormap(colors)

# axisbg=’w’
#bar colors
color=[[col[i-1] for i in anno.rank]]

fig,axes = plt.subplots(len(table)+1,1, sharex = True, sharey = False, figsize=(18,12),facecolor="white")

plt.subplots_adjust(left=None,bottom=None, right=0.8, top=None, wspace=0, hspace=0) 

for i in range(len(table)):
    ax=axes[i+1]
    table.iloc[i].plot(kind="bar",ax=ax,color=color,width=1)
#     ax.set_xlabel=("")
    ax.yaxis.set_label_position("right") 
    ax.set_ylabel(ylabel=table.iloc[i].name,rotation="horizontal",horizontalalignment="left")
    #ax.spines['right'].set_position(('data', 0))
    ax.set_autoscaley_on(True)
    ax.set_xticks([])
#     ax.set_xticklabels([])
    ax.set_yticks([])
   # ax.yaxis.set_visible(False)   
#     ax.xaxis.set_visible(False)   
    
ax=axes[0]
# ax.set_yticks([])
# ax.set_xticks([])
ax.imshow([anno.rank], interpolation='none', cmap=cmap, alpha=1)#,norm=norm
# ax.set_xticklabels(anno.index)mpatches.Patch
# ax.set_xticklabels(labels=ax.get_xticklabels(),rotation=45,ha="left",rotation_mode="anchor")
# ax.set_ylabel(ylabel=ax.get_ylabel(),rotation=90)
# ax.xaxis.tick_top()
# ax.set_ylabel("cluster",rotation='horizontal',ha="right",va="center")
# ax.set_xlabel(anno.col,);label"Level {l}".format(l=values[i])

# for legend

patches =[mpatches.Patch(color=col[i-1],label=np.unique(anno.iloc[:,0])[i-1]) for i in np.unique(anno.rank)]
# patches=[color=col,label=label]
box = ax.get_position()
ax.set_position([box.x0, box.y0, box.width , box.height])

# ax.legend(col,label,bbox_to_anchor=(1.3, 0.6),handleheight=1,fontsize=3)
ax.set_axis_off()
ax.legend(handles=patches, bbox_to_anchor=(1.16, 0.2),ncol=1,borderaxespad=0,fontsize=10)
fig.savefig(fname=sys.argv[3])
