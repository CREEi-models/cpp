
program main
    use cpp
    double precision cppinc, aape
    integer agecppstart, year, byear, id

    call loadcpp

    id = 1
    year = 2010
    byear = 1950
    agecppstart = 60
    cppinc = 0.0d0
    aape = 50000.0d0
    call getbenpaid(cppinc, aape,agecppstart,year,byear, id)
    write(*,*) cppinc
end program main
