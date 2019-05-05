#!/usr/bin/env python
# -*- coding: utf-8 -*- #
import sys
import os
import argparse
import sys
from Bio import SeqIO
import os.path
import os
parser = argparse.ArgumentParser(description="split novel annotated circRNA from novel_circRNAs.txt file and db files")
parser.add_argument('--genome',help="the genome fa file",required='True')
parser.add_argument('--n',help="chr number to plot",required='True')
parser.add_argument('--out_dir',help="out_dir",required='True')
parser.add_argument('--bed_file',help="bed_file",required='True')
parser.add_argument('--bin_length',help="bin_length",required='True')
argv = vars(parser.parse_args())
genome = argv['genome'].strip()
out_dir=argv['out_dir'].strip()
n=int(argv["n"].strip())
bed_file=argv['bed_file'].strip()
bin_length=int(argv["bin_length"].strip())
len_genome={}
chrom_count=0
for seq_record in SeqIO.parse(genome,'fasta'):
    chrom_count+=1
    if len(seq_record.seq) not in len_genome:
        len_genome[len(seq_record.seq)]=[]
    len_genome[len(seq_record.seq)].append(seq_record.id)
n=min(n,chrom_count)
b2s_len=reversed(sorted(len_genome.keys()))
genome_len={}
flag=0
for i in b2s_len:
        for j in len_genome[i]:
                genome_len[j]=i
                if len(genome_len) == n:
                        flag=1
                        break
        if flag == 1:
                break
total_len=sum(genome_len.values())
if total_len > 3116677:
        units=int(round(float(total_len)/3116677))*10000
else:
        units=10000

genome=[]
flag=0
for each in genome_len:
        if flag == 0:
                genome.append('chr - '+each+' '+each+' 0 '+str(genome_len[each]-1)+' vlgrey\n')
                flag=1
                continue
        if flag == 1:
                genome.append('chr - '+each+' '+each+' 0 '+str(genome_len[each]-1)+' grey\n')
                flag=0
open(out_dir+"/genome.txt",'w').writelines(genome)

bed_file_list=bed_file.strip().split(",")
karyotype_file=open(out_dir+"/genome.txt").readlines()

for each_bed_file in bed_file_list:
    bed_name=os.path.basename('each_bed_file').split(".")[0]
    bed_out=open(out_dir+"/"+bed_name+".out",'w')
    line_bed_file=open(each_bed_file).readlines()
    for eachline in karyotype_file:
        each=eachline.strip().split(" ")
        bin_number=int(each[5])/bin_length+1
        chr=each[2]
        for i in range(bin_number):
            number=0
            n=0
            for line in line_bed_file:
                line_list=line.strip().split("\t")
                start=line_list[1]
                n=line_list[3]
                n=line_list[3]
                gene_chr=line_list[0]
                if chr==gene_chr and float(i*bin_length)<=float(start)<=float((i+1)*bin_length):
                    number+=float(n)
            log_number=math.log10(number+1)/math.log10(2)
            bed_name.write(each[2]+"\t"+str(i*bin_length+1)+"\t"+str((i+1)*bin_length)+"\t"+str(log_number)+"\n")


#if len(bed_file_list)==1:
    

