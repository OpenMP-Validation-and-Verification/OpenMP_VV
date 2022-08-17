!===-test_ompx.F90-===/
!
!
! OpenMP API Version 5.2 July 2022
!
! Testing 'ompx' sentinel
!
! 'ompx' sentinel is reserved for
! implementation-defined extensions to
! fixed source form OpenMP directives.
!===-------------------------------===//
#include "ompvv.F90"

#define N 1024
PROGRAM test_ompx
        use iso_fortran_env
        use ompvv_lib
        use omp_lib
        implicit none

        OMPVV_TEST_OFFLOADING

        OMPVV_TEST_VERBOSE(test_fixed_ompx() .ne. 2)

        OMPVV_REPORT_AND_RETURN()
CONTAINS
        INTEGER FUNCTION test_fixed_ompx() 
                INTEGER :: i
                INTEGER :: errors
                INTEGER, DIMENSION(10) :: A
                INTEGER, DIMENSION(10) :: B
                INTEGER, DIMENSION(10) :: D
                errors = 0
                !$omp parallel num_threads(2)
                !$ompx test_nonexistant
                        errors = errors + 1
                !$omp end parallel
                test_fixed_ompx = errors
        END FUNCTION test_fixed_ompx
END PROGRAM test_ompx
