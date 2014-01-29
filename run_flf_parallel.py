#!/usr/bin/env python

# script to perform simple parallelization for bader and stephey field line following calculation

import sys, os, math

##############################################################
#user input block
runlabel='01282014' #this folder should already exist with points.in
numpoints=500 
numproc=5 
dphi=0.05
nmain=6
maxiter=100
imain=-150105
naux=6
vessel=1 #0 if no, 1 if yes
limiter=1 #number of limiters to include
divertor=0 #number of divertors to include

#will write results_0.out, results_1.out... for each processor

##############################################################

#number of points per proc
ptsperproc=numpoints/numproc

#move to the runs directory
os.chdir("/home/lastephey/flf/hsx_bs/runs/")
dir=str(runlabel)
#cd into that directory
os.chdir(dir)
#make python has zero-indexing so the folders will start at zero
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
	
	#write a flf.input file in each folder
	h=open('flf.input','w')
	h.write("! Automatically generated input file for Field Line Following Routines \n")
	h.write("! lines that begin with ! are comment lines \n")
	h.write("! Do not switch the order of things \n")
	h.write("! Do not delete anything unless you know what you are doing \n")
	h.write("! \n")
	h.write("! \n")
	h.write("! \n")
	h.write("! File to load data from \n")
	h.write("points.in \n")
	h.write("! Number of points to load from the file (must be less than the \n")
	h.write("! number of lines in the file) \n")
	h.write("%s \n" %(ptsperproc))
	h.write("! Distance to shift in each explicit step \n")
	h.write("%s \n" %(dphi))
	h.write("! Maximum number of iterations \n")
	h.write("%s \n" %(maxiter))
	h.write("! File to write data to \n")
	h.write("results_" + str(i) + ".out \n")
	h.write("! Number of main coils (c1.dat to cn.dat) \n")
	h.write("%s \n" %(nmain))
	h.write("! Current value in main coils, this is total main current, \n")
	h.write("! not current per winding \n")
	h.write("%s \n" %(imain))
	h.write("! File for auxiliary coils \n")
	h.write("aux_c.dat \n")
	h.write("! Number of aux coils (this needs to be the same or less than \n")
	h.write("! the first line in the aux coil file) \n")
	h.write("%s \n" %(naux))
	h.write("! taper values for aux coils, if the aux coil value is 0, do \n")
	h.write("! not include any values here \n")
	h.write("0.0 \n")
	h.write("0.0 \n")
	h.write("0.0 \n")
	h.write("0.0 \n")
	h.write("0.0 \n")
	h.write("0.0 \n")
	h.write("! Option of whether or not to include a vessel as a target \n")
	h.write("! followed by the name of the vessel file \n")
	h.write("%s \n" %(vessel))
	h.write("vessel.txt \n")
	h.write("! The number of limiters included in the calculation \n")
	h.write("! followed by the file locations for the limiters \n")
	h.write("%s \n" %(limiter))
	h.write("limiter.txt \n")
	h.write("! The number of divertors included in the calculation \n")
	h.write("! followed by the file names for the divertors \n")
	h.write("%s \n" %(divertor))
	h.write("!DIV_island4x25 \n")
	h.write("! Axis file, only needed if divertors exist \n")
	h.write("!mag_axis.dat \n")
	h.close
	
	#also, copy the pbs file into the current directory
	os.system("cp /home/lastephey/flf/hsx_bs/test.pbs test.pbs")
	
	#add symbolic links to current directory
	os.system("ln -sf /home/lastephey/flf/hsx_bs/limiter.txt .")
	os.system("ln -sf /home/lastephey/flf/hsx_bs/vessel.txt .")
	os.system("ln -sf /home/lastephey/flf/hsx_bs/c*.dat .")
	os.system("ln -sf /home/lastephey/flf/hsx_bs/aux_c.dat .")
	
	#now, call the pbs file to start the run
	os.system("qsub test.pbs")

		
		

		
	
		

