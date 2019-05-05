iimport sys
from scipy.stats import spearmanr
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
lst_2d=[]
for eachline in open('meanstpm.txt','r'):
    if eachline.startswith('ID'):
        lst_sample=eachline.strip().split('\t')[1:]
    else:
        lst1=eachline.strip().split('\t')[1:]    
        lst_2d.append(lst1)
bb,b=spearmanr(lst_2d)
data=np. ndarray.tolist(bb)

columns = lst_sample
rows = lst_sample
plt.figure(figsize=(12,12))
sns.set(style='dark', palette='dark', font='sans-serif', font_scale=1, color_codes=True, rc=None)  
plt.title('spearman Correlation of samples (r)', y=1.05, size=15)
sns.heatmap(data,linewidths=0.1,vmax=1.0, square=True, cmap='GnBu_r', linecolor='white', annot=True, xticklabels=columns, yticklabels=rows)

plt.show()
plt.savefig('spearman_Correlation_of_samples.png')
