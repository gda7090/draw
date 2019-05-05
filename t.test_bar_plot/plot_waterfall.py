#!/usr/bin/python
#-*- coding:utf-8 -*-

import sys
import matplotlib as mpl
import pandas as pd
import matplotlib.patches as mpatches
import matplotlib.pyplot as plt 
def plot_waterfall_graphs(ttest_file,xlabel):
    pdf=ttest_file + ".waterfall.pdf"
    df=pd.read_csv(ttest_file,sep="\t")
    df['diff']=df.iloc[:,1]-df.iloc[:,3]
    df.sort_values(by='diff', inplace=True,ascending=False)
    df.reset_index(inplace=True) 
    dffxls=ttest_file + ".diff.xls"
    df.iloc[:,1:].to_csv(dffxls, sep='\t', header=True, index=False)
    pv = df.iloc[:,6]
    df['colors'] = ['red' if x >= 0.05 else 'green' for x in pv]

    conf_interval_uphalf = abs(df.iloc[:,-3]-df.iloc[:,-2])
    # Draw plot
    plt.switch_backend('agg') #指定不需要GUI的backend（Agg, Cairo, PS, PDF or SVG）
    plt.figure(figsize=(16,8), dpi= 80)

    plt.vlines(x=df.index.values, ymin=0, ymax=df.iloc[:,-2], color=df.colors, linewidth=5)
    plt.errorbar(df.index.values,df.iloc[:,-2],yerr=conf_interval_uphalf,fmt='none',ecolor='grey',color='grey',elinewidth=1,capsize=2)

    # Decorations

    plt.gca().set(ylabel='Differences of mean frequency counts', xlabel=xlabel)
    plt.gca().xaxis.set_major_locator(mpl.ticker.FixedLocator(df.index.values))
    plt.gca().set_xticklabels(df.iloc[:,1],rotation=45,ha='right',fontsize=8)
    # plt.sexticks(df.index, df.cars, fontsize=12)
    colors=["red","green"]
    labels=["$P$ = ns","$P$ < 0.05"]
    patches = [mpatches.Patch(color=colors[i],label=labels[i]) for i in range(len(colors))]
    # plt.legend(handles=patches, loc="upper right", borderaxespad=0.,nrow=1)
    plt.legend(handles=patches, loc="upper right", borderaxespad=1,ncol=2)
    # plt.title('Diverging Bars of Car Mileage', fontdict={'size':20})

    plt.grid(linestyle='--', alpha=0.5)
    plt.gca().margins(x=0.01, y=0.03)
    # plt.gca().spines['right'].set_position(('data', len(df.index.values)))
    # plt.gca().spines['left'].set_position(('data',-1))

    # plt.show()
    plt.savefig(pdf)
    
if __name__ == "__main__":
    with open(sys.argv[1], 'r') as f:
        for line in f.readlines():
            plot_waterfall_graphs(line.strip(),sys.argv[2])
