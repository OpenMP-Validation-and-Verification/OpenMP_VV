!===----test_loop_iterator_lastprivate.F90----------------------------------===//
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
    integer :: errors_none, errors_parallel, errors_teams

    OMPVV_TEST_OFFLOADING

    errors_none = test_loop_lastprivate_none()
    errors_parallel = test_loop_lastprivate_parallel()
    errors_teams = test_loop_lastprivate_target_teams()

    OMPVV_ERROR_IF(errors_none /= 0, "Lastprivate failed with just the loop construct.")
    OMPVV_ERROR_IF(errors_parallel /= 0, "Lastprivate failed with the parallel construct.")
    OMPVV_ERROR_IF(errors_teams /= 0, "Lastprivate failed with parallel and target teams.")

    OMPVV_REPORT_AND_RETURN()

contains

    integer function test_loop_lastprivate_none() result(errors)
        real :: a(m,n)
        integer :: i, j

        !$omp loop bind(thread)
        do j = 1, n
            do i = 1, m
                a(i,j) = 10*i + j
            end do
        end do
        !$omp end loop

        errors = merge(1, 0, i /= m+1) &
               + merge(1, 0, j /= n+1)

        associate(aseq => reshape( &
            [((10*i + j, i=1,m), j=1,n)], [m,n]))
            errors = errors + merge(1, 0, any(a /= aseq))
        end associate

    end function test_loop_lastprivate_none


    integer function test_loop_lastprivate_parallel() result(errors)
        real :: a(m,n)
        integer :: i, j

        !$omp parallel
        !$omp loop
        do j = 1, n
            do i = 1, m
                a(i,j) = 10*i + j
            end do
        end do
        !$omp end loop
        !$omp end parallel

        errors = merge(1, 0, i /= m+1) &
               + merge(1, 0, j /= n+1)

        associate(aseq => reshape( &
            [((10*i + j, i=1,m), j=1,n)], [m,n]))
            errors = errors + merge(1, 0, any(a /= aseq))
        end associate

    end function test_loop_lastprivate_parallel


    integer function test_loop_lastprivate_target_teams() result(errors)
        real :: a(m,n)
        integer :: i_last(n)
        integer :: i, j

        !$omp target teams num_teams(4) map(from: a, i_last)
        !$omp loop bind(teams)
        do j = 1, n
            block
                integer :: i

                !$omp parallel
                !$omp loop
                do i = 1, m
                    a(i,j) = 10*i + j
                end do
                !$omp end loop
                !$omp end parallel

                i_last(j) = i
            end block
        end do
        !$omp end loop
        !$omp end target teams

        errors = merge(1, 0, j /= n+1) &
               + count(i_last /= m+1)

        associate(aseq => reshape( &
            [((10*i + j, i=1,m), j=1,n)], [m,n]))
            errors = errors + merge(1, 0, any(a /= aseq))
        end associate

    end function test_loop_lastprivate_target_teams

end program test_loop_lastprivate