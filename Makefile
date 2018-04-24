# the output file will be called

EXT = cpython-36m-darwin.so
RUN = runtime
F90 = f2py
FFLAGS = -m cpp --quiet --f90flags='-Ofast'
all : cpp.$(EXT) 

cpp.$(EXT) : src/cpp.f95
	$(F90) $(FFLAGS) -c  $< 
	mv cpp.$(EXT) cpp.$(EXT).dSYM runtime/.
# clean up
clean:
	rm -rf runtime/cpp.$(EXT) runtime/cpp.$(EXT).dSYM 
