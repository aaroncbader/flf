#!/usr/bin/env python

#script to merge all results files into one large results.out file
#should probably be located in hsx_bs and the path adjusted accordingly

import sys, os, math, glob, fileinput

#this is the path to the directory that contains the results_n.out files you want to merge
path='/mnt/K_drive/Field_Line_Following_New/hsx_bs/runs/01302014/2mmneg/'

numfiles=len(glob.glob1(path,'results_*.out'))

#writes a single results.out file
fileout=path+'results.out'

with open(fileout,'w') as fout:
	for i in range(numfiles):
		for line in fileinput.input(path+'results_%s.out' %(i)):
			fout.write(line)
		
		
