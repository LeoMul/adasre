FC=gfortran
FFLAGSPROD  = -O3 -fopenmp 
FFLAGSDEBUG = -g -fbacktrace -fcheck=all -Wall 

SRC= \
    src/kinds.f90\
    src/tempgrid.f90\
    src/constants.f90\
    src/reference_data.f90\
    src/variables.f90\
    src/configs.f90\
    src/adf04.f90\
    src/initdeinit.f90\
    src/input.f90\
    src/postprocess.f90\
    src/readoic.f90\
    src/readblock.f90\
    src/adasre.f90\
    src/upsilon.f90\
    src/collstrength.f90\
    src/coredr.f90

OBJ=${SRC:.f90=.o}
PROGRAM = adasre
PRG_OBJ = $(PROGRAM).o

%.o: %.f90
	$(FC) $(FFLAGSPROD) -c -o $@ $<

adasre: $(OBJ)
	$(FC) $(FFLAGSPROD) -o $@ $(OBJ)

clean:
	rm *.o *.mod adasre src/*.o  
