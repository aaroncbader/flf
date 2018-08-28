
FC = mpif90


# End of system-dependent variable assignments

TARGET = flf
FC = mpif90 
EXTRA_COMPILE_FLAGS = -fopenmp -O2 -I/usr/local/lib -I/usr/include -ffree-line-length-none -fdefault-real-8 -fdefault-double-8 
#EXTRA_LINK_FLAGS = -fopenmp
EXTRA_LINK_FLAGS =  -fopenmp -L/usr/local/lib -lnetcdff  -lnetcdf 

export

.PHONY: all clean

all: $(TARGET)

include makefile.depend

%.o: %.f90 
	$(FC) $(EXTRA_COMPILE_FLAGS) $(EXTRA_LINK_FLAGS) -c $<

%.o: %.f 
	$(FC) $(EXTRA_COMPILE_FLAGS)-c $<

lib$(TARGET).a: $(OBJ_FILES)
	ar rcs lib$(TARGET).a $(OBJ_FILES)

$(TARGET): lib$(TARGET).a $(TARGET).o
	$(FC) -o $(TARGET) $(TARGET).o lib$(TARGET).a $(EXTRA_LINK_FLAGS)


clean:
	rm -f *.o *.mod *.MOD *~ $(TARGET) *.a

