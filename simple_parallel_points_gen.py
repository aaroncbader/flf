#!/usr/bin/env python

# script to perform simple parallelization for bader and stephey field line following calculation

# script will read input file, number of processors desired, total number of points, and allocate the points appropriately on each processor

#script to use create simple parallel runs for the bader and stephey flf calculation

import sys, os, math

##############################################################
#user input block
inputfile='flf.input'
runlabel='test' #this folder should already exist with points.in
numpoints=500 #make sure you adjust flf.input for the correct number
numproc=5 #in flf.input, it's numpoints/numproc

##############################################################

#number of points per proc
ptsperproc=numpoints/numproc

#move to the runs directory
os.chdir("/home/lastephey/flf/hsx_bs/runs/")
dir=str(runlabel)
#cd into that directory
os.chdir(dir)
#make the folders for the point groups
#note: python has zero-indexing so the folders will start at zero
for i in range(numproc):
	dir=str(i)
	#remove if it already exists
	os.system("rm -rf " +dir)
	os.mkdir(dir)

	
#move back to the parent directory
os.chdir("/home/lastephey/flf/hsx_bs/runs/%s" %(runlabel))

#open points file and read data
f=open('points.in','r')
points=f.readlines()
f.close

for i in range(numproc):
	#cd into the correct folder
	os.chdir("/home/lastephey/flf/hsx_bs/runs/%s/%s" %(runlabel,i))
	start=i*ptsperproc
	end=(i+1)*ptsperproc
	pgroup=points[start:end]
	#write a points.in file in each folder
	g=open('points.in','w')
	g.write("".join(pgroup))
	g.close
	#now that we have the points file, copy in the flf.input file
	#make sure this has the right number of points per processor
	os.system("cp /home/lastephey/flf/hsx_bs/runs/%s/flf.input flf.input" %(runlabel))
	#also, copy the pbs file into the current directory
	os.system("cp /home/lastephey/flf/hsx_bs/runs/%s/test.pbs test.pbs" %(runlabel))
	#add symbolic links to current directory
	os.system("ln -sf /home/lastephey/flf/hsx_bs/limiter.txt .")
	os.system("ln -sf /home/lastephey/flf/hsx_bs/vessel.txt .")
	os.system("ln -sf /home/lastephey/flf/hsx_bs/c*.dat .")
	os.system("ln -sf /home/lastephey/flf/hsx_bs/aux_c.dat .")
	#now, call the pbs file to start the run
	os.system("qsub test.pbs")

		
		

		
	
		

