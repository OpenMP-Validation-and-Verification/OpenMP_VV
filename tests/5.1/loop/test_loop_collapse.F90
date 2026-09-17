!===----test_workdistribute_directive.F90-----------------------------------===//
!
! OpenMP API Version 6.0 Nov 2024
! Pg. 901
! This test checks if the workdistribute construct works with target teams in
! various cases which involve array manipulations. The workdistribute Construct 
! example in the 6.0.1 examples document was referenced.
!===------------------------------------------------------------------------===//

#include "ompvv.F90"

program test_loop_collapse
    use iso_fortran_env
    use ompvv_lib
    use omp_lib
    implicit none

    integer, parameter :: m = 32, n = 10
    real :: a(m,n)
    integer :: i, j

    ! Section 5.1.1, pg. 97, line 16-17
    ! The loop iteration variables in the associated loops of a
    ! simd construct with multiple associated loops are lastprivate.

    !$omp loop collapse(2) bind(thread)
    do j = 1, n
    do i = 1, m
        a(i,j) = 10*i + j
    end do
    end do

    if (i /= m+1) error stop 1
    if (j /= n+1) error stop 2

    associate(aseq => reshape([((10*i + j,i=1,m),j=1,n)],[m,n]) )
        if ( any(a /= aseq) ) error stop 3
    end associate

    !OMPVV_ERROR_IF(errors /= 0, "The workdistribute directive did not perform as expected.")
    print *, "PASS"
    OMPVV_REPORT_AND_RETURN()

end program