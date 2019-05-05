#!/usr/bin/python
#-*- coding:utf-8 -*-

import sys
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def plot3Dbar(TRBvjpairs,outpdf):
    vab={}
    with open(TRBvjpairs, 'r') as f:
        first_line = next(f)# skip first line
        [v,j,ab] = first_line.strip().split('\t')
        lines = f.readlines()
        for line in lines:
            tmps=line.strip().split('\t')
            if tmps[0] in vab:
                vab[tmps[0]][tmps[1]]=tmps[2]
            else:
                tmp={}
                tmp[tmps[1]]=tmps[2]
                vab[tmps[0]]=tmp
    f.close()
    yticks=[]
    for i in list(vab.values()):
        yticks.extend(list(i.keys()))
    yticks = list(set(yticks))
    yticks.sort()

    xticks = list(vab.keys())
    xticks.sort()


    ytick = list(range(len(yticks)))
    mpl.rcParams['font.size'] = 5
    plt.switch_backend('agg')
    fig = plt.figure()
    ax = fig.gca(projection='3d')  # get current axes
    cmap = mpl.cm.tab20
    colors = [mpl.colors.rgb2hex(cmap.colors[i+1]) for i in ytick]

    for z in ytick:
        xs = list(range(len(xticks)))
        ys = [float(vab[i][yticks[z]]) if (yticks[z] in vab[i].keys()) else 0.0 for i in xticks]
        color =plt.cm.Set2(list(range(plt.cm.Set2.N)))
        ax.bar(xs, ys, zs=z, zdir='y', color=colors[z], alpha=0.8)

    ax.xaxis.set_major_locator(mpl.ticker.FixedLocator(xs))
    ax.yaxis.set_major_locator(mpl.ticker.FixedLocator(ytick))

    ax.set_xlabel(v)
    ax.set_ylabel(j)
    ax.set_zlabel(ab)

    ax.set_xticklabels(xticks,rotation=10,ha='right',va='bottom')
    ax.set_yticklabels(yticks,rotation=10, ha='left', va='bottom'  )
    # Vertical alignment must be one of ('top', 'bottom', 'center', 'baseline')
    plt.savefig(outpdf)

if __name__ == "__main__":
    with open(sys.argv[1], 'r') as f:
        for line in f.readlines():
            pdf=line.strip().split('/')[-1] + ".pdf"
            plot3Dbar(line.strip(),pdf)  

