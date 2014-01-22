FC = gfortran

OP = -fdefault-real-8 -fdefault-double-8 -fbacktrace -fbounds-check  -O2

FF = follow_field.o

CP = $(FC) $(OP) -c

MO = 	utility.o 	coil_module.o	div_module.o 	read_coils.o\
	points_module.o	get_points.o 	limiter_module.o compute_bs.o\
	dlsode.o	vessel_module.o inside_vessel.o inside_div.o\
	follow_field.o	inside_limiter.o  

WC =	coil_module.o	read_coils.o

TBS = 	coil_module.o 	points_module.o div_module.o 	vessel_module.o\
	utility.o 	inside_vessel.o inside_div.o\
	read_coils.o\
	compute_bs.o

TV = 	utility.o 	vessel_module.o inside_vessel.o

TP = 	points_module.o	get_points.o

TD =    utility.o 	div_module.o inside_div.o

TL =    vessel_module.o utility.o inside_vessel.o boxport_limiter_check.o

RSI =   parser.o

PROGRAMS = follow_to_wall write_coils test_vessel test_bs test_points

#This is the default
follow_to_wall: $(MO)
	$(FC) $(OP) follow_to_wall.f90 $(MO) -o follow_to_wall

follow_to_limiter: $(MO)
	$(FC) $(OP) follow_to_limiter.f90 $(MO) -o follow_to_limiter


all_tests:
	make test_write
	make test_bs
	make test_vessel
	make test_points
	make test_divread
	make test_limiter

test_write: $(WC)
	$(FC) $(OP) write_coils.f90 $(WC) -o write_coils

test_bs: $(TBS)
	$(FC) $(OP) test_bs.f90 $(TBS) -o test_bs

test_vessel: $(TV)
	$(FC) $(OP) test_vessel.f90 $(TV) -o test_vessel

test_points: $(TP)
	$(FC) $(OP) test_points.f90 $(TP) -o test_points

test_divread: $(TD)
	$(FC) $(OP) test_divread.f90 $(TD) -o test_divread

test_limiter: $(TL)
	$(FC) $(OP) test_limiter.f90 $(TL) -o test_limiter

test_parser: $(RSI)
	$(FC) $(OP) read_sample_input.f90 $(RSI) -o read_sample_input


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
get_points.o:	get_points.f90
	$(CP)	get_points.f90
boxport_limiter_check.o: boxport_limiter_check.f90
	$(CP)	boxport_limiter_check.f90
parser.o:	parser.f90
	$(CP)	parser.f90



clean:
	rm -f *.o *.mod
	rm $(PROGRAMS)
