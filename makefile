FC = gfortran

OP = -fdefault-real-8 -fdefault-double-8 -fbacktrace -fbounds-check  -g

FF = follow_field.o

CP = $(FC) $(OP) -c

MO = 	coil_module.o	read_coils.o	compute_bs.o\
	dlsode.o	vessel_module.o inside_vessel.o\
	points_module.o	get_points.o 	follow_field.o

WC =	 coil_module.o	read_coils.o

TBS = 	coil_module.o 	read_coils.o 	compute_bs.o

TV = 	vessel_module.o inside_vessel.o

TP = 	points_module.o	get_points.o

PROGRAMS = follow_to_wall write_coils test_vessel test_bs test_points

#This is the default
follow_to_wall: $(MO)
	$(FC) $(OP) follow_to_wall.f90 $(MO) -o follow_to_wall


all_tests:
	make test_write
	make test_bs
	make test_vessel
	make test_points

test_write: $(WC)
	$(FC) $(OP) write_coils.f90 $(WC) -o write_coils

test_bs: $(TBS)
	$(FC) $(OP) test_bs.f90 $(TBS) -o test_bs

test_vessel: $(TV)
	$(FC) $(OP) test_vessel.f90 $(TV) -o test_vessel

test_points: $(TP)
	$(FC) $(OP) test_points.f90 $(TP) -o test_points



follow_field.o:	follow_field.f90
	$(CP)	follow_field.f90
coil_module.o:	coil_module.f90
	$(CP)	coil_module.f90
read_coils.o:	read_coils.f90
	$(CP)	read_coils.f90
compute_bs.o:	compute_bs.f90
	$(CP)	compute_bs.f90
dlsode.o:	dlsode.f
	$(CP)	dlsode.f
vessel_module.o:	vessel_module.f90
	$(CP)	vessel_module.f90
inside_vessel.o:	inside_vessel.f90
	$(CP)	inside_vessel.f90
points_module.o:	points_module.f90
	$(CP)	points_module.f90
get_points.o:	get_points.f90
	$(CP)	get_points.f90

clean:
	rm -f *.o
	rm $(PROGRAMS)
