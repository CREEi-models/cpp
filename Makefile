# define matlab dir
MDIR = /Applications/MATLAB_R2017b.app

# compiles mex files using gfortran
F90 = gfortran

# compiler flags for gfortran
FFLAGS = -Ofast -cpp -fPIC 

# where to find objects
OBJ=obj

# where to output executables
OUT=runtime

# Figure out which platform we're on
UNAME = $(shell uname -s)

# Linux
ifeq ($(findstring Linux,${UNAME}), Linux)
	# define which files to be included
	FINCLUDE = -I$(MDIR)/extern/include  -shared 
	# define extension
	EXT = mexa64
endif

# Mac OS X
ifeq ($(findstring Darwin,${UNAME}), Darwin)
	# define which files to be included
	FINCLUDE = -L$(MDIR)/bin/maci64 -I$(MDIR)/extern/include -J$(OBJ)  -lmx -lmex -lmat
	# define extension
	EXT = mexmaci64
    ifeq ($(F90), ifort)
        FNOMAIN = -nofor_main -bundle
    endif
    ifeq ($(F90), gfortran)
        FNOMAIN = -shared
    endif
endif

# the output file will be called
all : $(OBJ)/cpp.o $(OUT)/cppben.$(EXT) 
$(OBJ)/cpp.o: src/cpp.f95
	$(F90) $(FFLAGS) $(FINCLUDE)  -c  $< -o $@

# copying data between Matlab and Fortran 
$(OBJ)/cppben.o: src/cppben.f95 
	$(F90) $(FFLAGS) $(FINCLUDE)  -c  $< -o $@

# creating the mexa64 file that Matlab can communicate with
$(OUT)/cppben.$(EXT): $(OBJ)/cppben.o   
	$(F90) $(FFLAGS) $(FINCLUDE)  $(FNOMAIN) $(OBJ)/cpp.o $(OBJ)/cppben.o   -o $@

# clean up
clean:
	rm -f $(OBJ)/* $(OUT)/cppben.$(EXT)
