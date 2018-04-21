#include "fintrf.h"

! standard header for mex functions
subroutine mexfunction(nlhs, plhs, nrhs, prhs)
    !use cpp
    ! number of input arguments, number of output arguments
    integer      :: nlhs, nrhs
    ! pointer to inputs and outputs
    mwPointer    :: plhs(*), prhs(*) 
    ! get some of the matlab mex functions
    mwPointer :: mxGetPr, mxCreateDoubleMatrix
    ! define a size integer so that we can get its type
    mwSize :: m = 1

    ! output as real
    real*8 :: number_in
    real*8 :: number_out 

    ! copy the right-hand side argument in matlab to number_in
    call mxCopyPtrToReal8(mxGetPr(prhs(1)), number_in, m)

    ! load cpp 
    !call loadcpp

    ! do a simple multiplication
    number_out = number_in * 2.0
 
    ! initialize the left-hand side of the function as a 1-by-1 matrix
    plhs(1) = mxCreateDoubleMatrix(m, m, 0)
    
    ! copy the output to the left-hand side
    call mxCopyReal8ToPtr(number_out, mxGetPr(plhs(1)), 1) 
   
end subroutine
