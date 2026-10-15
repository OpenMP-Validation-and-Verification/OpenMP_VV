!===----test_loop_iterator_lastprivate.F90-----------------------------------===//
!
! OpenMP API Version 6.0 Nov 2024
! Pg. 901
! This test checks if the iterator variable observes lastprivate behavior inside
! of the loop construct.
!===------------------------------------------------------------------------===//

#include "ompvv.F90"

program test_loop_lastprivate
    use iso_fortran_env
    use ompvv_lib
    use omp_lib
    implicit none

    integer, parameter :: m = 32, n = 10
    real :: a(m,n)
    integer :: i, j, errors = 0

    !!$omp loop collapse(2)

    !!$omp target teams
    !$omp parallel
    !$omp loop collapse(2)
    do j = 1, n
    do i = 1, m
        a(i,j) = 10*i + j
    end do
    end do

    if (i /= m+1) errors = errors + 1
    if (j /= n+1) errors = errors + 1

    associate(aseq => reshape([((10*i + j,i=1,m),j=1,n)],[m,n]))
        if ( any(a /= aseq) ) OMPVV_WARNING("The shape of the array a is not as expected.")
    end associate

    OMPVV_ERROR_IF(errors /= 0, "The lastprivate behavior was not observed as expected.")
    OMPVV_REPORT_AND_RETURN()
end program test_loop_lastprivate
