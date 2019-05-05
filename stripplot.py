#!/usr/bin/python
#-*- coding:utf-8 -*-
import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
import sys

def stripplot(abu,dn,gn,ylabel):
    # Import Data
    df = pd.read_csv(abu,sep="\t")
    plt.switch_backend('agg')
    plt.figure(figsize=(6,3), dpi= 80)
    sns.stripplot(x=df.iloc[:,gn-1], y=df.iloc[:,dn-1]*100, data=df, 
                  color='white', edgecolor='grey',
                  linewidth =0.5, size=3, jitter=1)
    plt.gca().set(ylabel=ylabel,xlabel="")
    plt.grid(linestyle='--', alpha=0.5)

    pdf=abu+".pdf"
    plt.savefig(pdf)

if __name__ == "__main__":
    abu,dn,gn,ylabel=sys.argv[1:]
    stripplot(abu,int(dn),int(gn),ylabel)
