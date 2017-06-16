
#F90=mpif90 -warn all -std08 #-openmp
F90=gfortran6 -std=f2008 -Wall -pedantic -march=native -funroll-loops -O0 -finline-limit=60 -fbacktrace -ffpe-trap=underflow,overflow,denormal -fbounds-check
F90OPT=

AFNLPATH=/home/alberto/code/fortran/afnl/ 
GAPATH=/home/alberto/code/fortran/libga/
AFNLNAME=f90 
GANAME=ga

VPATH=src:main
SRCDIR=$(VPATH)
INC=include/

OBJ = uncertainties.o uncertainties_arith.o uncertainties_func.o

all: $(OBJ) check1.x check2.x

uncertainties.o: uncertainties.f90
	$(F90) -J $(INC) -c $^ -o $(INC)/$@ -I$(INC)

uncertainties_arith.o: uncertainties_arith.f90
	$(F90) -J $(INC) -c $^ -o $(INC)/$@ -I$(INC)

uncertainties_func.o: uncertainties_func.f90
	$(F90) -J $(INC) -c $^ -o $(INC)/$@ -I$(INC)

check1.x:  check1.f90  $(OBJ)
	$(F90) -J $(INC) $(addprefix $(INC),$(OBJ)) $< -o $@ -I$(INC)

check2.x:  check2.f90  $(OBJ)
	$(F90) -J $(INC) $(addprefix $(INC),$(OBJ)) $< -o $@ -I$(INC)

clean:
	rm $(INC)/*.o 
	rm $(INC)/*.mod 
	rm $(INC)/*.smod 
	rm check1.x check2.x
