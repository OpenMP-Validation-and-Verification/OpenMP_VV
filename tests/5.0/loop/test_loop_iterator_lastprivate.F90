!===----test_loop_iterator_lastprivate.F90----------------------------------===//
!
! OpenMP API Version 6.0 Nov 2024
! Pg. 901
! This test checks if the iterator variable observes lastprivate behavior inside
! of the loop construct.
!===------------------------------------------------------------------------===//

#include "ompvv.F90"

program test_loop_lastprivate_driver
    use iso_fortran_env
    use ompvv_lib
    use omp_lib
    implicit none

    integer :: errors = 0

    ! Test all combinations of the two IF clauses.
    if (.not. test_loop_lastprivate(.false., .false.)) errors = errors + 1
    if (.not. test_loop_lastprivate(.false., .true.))  errors = errors + 1
    if (.not. test_loop_lastprivate(.true.,  .false.)) errors = errors + 1
    if (.not. test_loop_lastprivate(.true.,  .true.))  errors = errors + 1

    OMPVV_ERROR_IF(errors /= 0, &
        "The lastprivate behavior was not observed as expected.")

    OMPVV_REPORT_AND_RETURN()

contains

    logical function test_loop_lastprivate(parallel_if, target_teams_if) &
        result(passed)

        logical, intent(in) :: parallel_if, target_teams_if

        integer, parameter :: m = 32, n = 10
        real :: a(m,n)
        integer :: i, j

        passed = .true.

        !$omp target teams if(target: target_teams_if) &

        !$omp& num_teams(4) map(tofrom: a, i, j)
        !$omp loop bind(teams)
        do j = 1, n
            !$omp parallel if(parallel_if)
            !$omp loop
            do i = 1, m
                a(i,j) = 10*i + j
            end do
            !$omp end loop
            !$omp end parallel
        end do
        !$omp end loop
        !$omp end target teams

        ! Check the implicit lastprivate behavior.
        if (i /= m+1 .or. j /= n+1) then
            passed = .false.
        end if

        ! Check the array contents.
        associate(aseq => reshape( &
            [((10*i + j, i=1,m), j=1,n)], [m,n]))
            if (any(a /= aseq)) then
                OMPVV_WARNING("The shape of the array a is not as expected.")
                passed = .false.
            end if
        end associate

    end function test_loop_lastprivate

end program test_loop_lastprivate_driver
