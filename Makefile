FC=gfortran
FFLAGSPROD  = -O3 
FFLAGSDEBUG = -g -fbacktrace -fcheck=all -Wall 

SRC= \
    src/reference_data.f90\
    src/variables.f90\
    src/configs.f90\
    src/adf04.f90\
    src/initdeinit.f90\
    src/input.f90\
    src/readoic.f90\
    src/readblockform.f90\
    src/adasre.f90\
    src/upsilon.f90 

OBJ=${SRC:.f90=.o}
PROGRAM = adasre
PRG_OBJ = $(PROGRAM).o

%.o: %.f90
	$(FC) $(FFLAGSPROD) -c -o $@ $<

adasre: $(OBJ)
	$(FC) $(FFLAGSPROD) -o $@ $(OBJ)

clean:
	rm *.o *.mod adasre src/*.o src/*.mod 