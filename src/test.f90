program main
	integer i, j
	logical x(10)
	double precision y(10)
	x(:) = .false.
	x(5) = .true.
	x(10) = .true.
	do i = 1, 10, 1
		y(i) = dble(i)
	end do
	
	do i = 1, 5, 1
		j = minloc(y,mask=x)
		write(*,*) j
	end do
end program main
