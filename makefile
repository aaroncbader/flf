FC = gfortran

OP = -fdefault-real-8 -fdefault-double-8 -fbacktrace -fbounds-check -O2

CP = $(FC) $(OP) -c

MO = 	utility.o 	coil_module.o	div_module.o 	read_coils.o\
	points_module.o	mgrid_module.o  get_points.o 	limiter_module.o\
	options_module.o  compute_bs.o    read_mgrid.o\
	dlsode.o	vessel_module.o inside_vessel.o	lcfs_module.o\
	inside_div.o	diffusion.o	randomize.o	lcfs.o\
	follow_field.o	inside_limiter.o  parser.o 	read_input.o\
	read_namelist.o follow_to_wall.o

EPS = 	utility.o 	coil_module.o	div_module.o 	read_coils.o\
	points_module.o	mgrid_module.o  get_points.o 	limiter_module.o\
	options_module.o  compute_bs.o    read_mgrid.o\
	dlsode.o	vessel_module.o inside_vessel.o	lcfs_module.o\
	inside_div.o	diffusion.o	randomize.o	lcfs.o\
	follow_field.o	inside_limiter.o  parser.o 	read_input.o\
	eps_module.o eps_eff.o

BM =    utility.o 	coil_module.o	div_module.o 	read_coils.o\
	points_module.o	mgrid_module.o  get_points.o 	limiter_module.o\
	options_module.o  compute_bs.o    read_mgrid.o\
	dlsode.o	vessel_module.o inside_vessel.o\
	inside_div.o    lcfs_module.o  	lcfs.o\
	follow_field.o	inside_limiter.o  parser.o 	read_input.o\
	Bmag.o

CT = 	utility.o 	coil_module.o	div_module.o 	read_coils.o\
	points_module.o	get_points.o 	limiter_module.o options_module.o\
	compute_bs.o	lcfs_module.o\
	dlsode.o	vessel_module.o inside_vessel.o randomize.o\
	inside_div.o	diffusion.o	lcfs.o\
	follow_field.o	inside_limiter.o  parser.o 	read_input.o\
	comprehensive_test.o

WC =    utility.o       coil_module.o   div_module.o    read_coils.o\
        points_module.o get_points.o    limiter_module.o options_module.o\
        mgrid_module.o  read_mgrid.o    compute_bs.o 	lcfs_module.o	lcfs.o\
        dlsode.o        vessel_module.o inside_vessel.o\
        inside_div.o\
        follow_field.o  inside_limiter.o  parser.o      read_input.o\
	write_coils.o


PROGRAMS = follow_to_wall Bmag comprehensive_test eps_eff

#This is the default
follow_to_wall: $(MO)
	$(FC) $(OP) -o follow_to_wall $(MO) 

eps_eff: $(EPS)
	$(FC) $(OP) -o eps_eff $(EPS) 

Bmag: $(BM)
	$(FC) $(OP) -o Bmag $(BM)


comp_test: $(CT)
	$(FC) $(OP) -o comprehensive_test $(CT)

write_coils: $(WC)
	$(FC) $(OP) -o write_coils $(WC)


utility.o:	utility.f90
	$(CP)	utility.f90
div_module.o:	div_module.f90
	$(CP)	div_module.f90
inside_div.o: 	inside_div.f90
	$(CP)   inside_div.f90
limiter_module.o:	limiter_module.f90
	$(CP)	limiter_module.f90
inside_limiter.o:	inside_limiter.f90
	$(CP)		inside_limiter.f90
points_module.o:	points_module.f90
	$(CP)	points_module.f90
lcfs_module.o:	lcfs_module.f90
	$(CP)	lcfs_module.f90
follow_field.o:	follow_field.f90
	$(CP)	follow_field.f90
coil_module.o:	coil_module.f90
	$(CP)	coil_module.f90
mgrid_module.o:	mgrid_module.f90
	$(CP)	mgrid_module.f90
eps_module.o:	eps_module.f90
	$(CP)	eps_module.f90
read_coils.o:	read_coils.f90
	$(CP)	read_coils.f90
read_mgrid.o:	read_mgrid.f90
	$(CP)	read_mgrid.f90
options_module.o:	options_module.f90
	$(CP)	options_module.f90
compute_bs.o:	compute_bs.f90
	$(CP)	compute_bs.f90
dlsode.o:	dlsode.f
	$(CP)	dlsode.f
vessel_module.o:	vessel_module.f90
	$(CP)	vessel_module.f90
inside_vessel.o:	inside_vessel.f90
	$(CP)	inside_vessel.f90
randomize.o:	randomize.f90
	$(CP)	randomize.f90
diffusion.o:	diffusion.f90
	$(CP)	diffusion.f90
get_points.o:	get_points.f90
	$(CP)	get_points.f90
boxport_limiter_check.o: boxport_limiter_check.f90
	$(CP)	boxport_limiter_check.f90
parser.o:	parser.f90
	$(CP)	parser.f90
read_input.o:	read_input.f90
	$(CP)	read_input.f90
read_namelist.o:	read_namelist.f90
	$(CP)	read_namelist.f90
write_coils.o:	write_coils.f90
	$(CP)	write_coils.f90
lcfs.o:		lcfs.f90
	$(CP)	lcfs.f90
Bmag.o:		Bmag.f90
	$(CP)	Bmag.f90
comprehensive_test.o:	comprehensive_test.f90
	$(CP)	comprehensive_test.f90
follow_to_wall.o:	follow_to_wall.f90
	$(CP)	follow_to_wall.f90
eps_eff.o:	eps_eff.f90
	$(CP)	eps_eff.f90


clean:
	rm -f *.o *.mod
	rm $(PROGRAMS)
