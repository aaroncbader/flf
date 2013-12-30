FC = gfortran

OP = -fdefault-real-8 -fdefault-double-8 -O2 -fbacktrace

FF = follow_field.o

CP = $(FC) $(OP) -c

MO = 	coil_module.o	read_coils.o	compute_bs.o\
	dlsode.o	inside_vessel.o

all: $(MO)
	$(FC) follow_field.f90 $(MO) -o follow_field

#follow_field.o:	follow_field.f90
#	$(CP)	follow_field.f90
coil_module.o:	coil_module.f90
	$(CP)	coil_module.f90
read_coils.o:	read_coils.f90
	$(CP)	read_coils.f90
compute_bs.o:	compute_bs.f90
	$(CP)	compute_bs.f90
dlsode.o:	dlsode.f
	$(CP)	dlsode.f
inside_vessel.o:	inside_vessel.f90
	$(CP)	inside_vessel.f90

clean:
	rm -f *.o
