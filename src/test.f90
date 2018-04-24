module test
	implicit none
	
	contains

	subroutine this(one,two)
		integer, intent(in)::one
		integer, intent(out)::two

		two = 2*one

	end subroutine this

	end module test