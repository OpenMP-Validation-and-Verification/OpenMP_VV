!===--- test_target_mapping_before_alloc.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! The description of the map clause was modified to clarify the mapping
! order when multiple map-types are specified for a variable or structure
! members of a variable on the same construct.
!
! For a given construct, the effect of a map clause with the to, from, or
! tofrom map-type is ordered before the effect of a map clause with the
! alloc map-type.
!
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_mapping_before_alloc
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(to_before_alloc() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION to_before_alloc()
    INTEGER :: errors, x, scalar
    INTEGER,DIMENSION(N) :: a

    TYPE structure
       INTEGER :: var
       INTEGER,DIMENSION(N) :: b
    END TYPE structure

    TYPE(structure) :: test_struct
    test_struct%var = 1

    DO x = 1, N
       a(x) = x
       test_struct%b(x) = x
    END DO

    scalar = 80
    errors = 0

    !$omp target map(alloc: scalar, a, test_struct) map(to: scalar, a, &
    !$omp& test_struct) map(tofrom: errors)
    !OMPVV_TEST_AND_SET_VERBOSE(errors, scalar .ne. 80 .OR. a(2) .ne. 2 .OR. test_struct%var .ne. 1 .OR. test_struct%b(2) .ne. 2)
    OMPVV_TEST_AND_SET(errors, scalar .ne. 80 .OR. a(2) .ne. 2 .OR. test_struct%var .ne. 1 .OR. test_struct%b(2) .ne. 2)

    !$omp end target

    to_before_alloc = errors
  END FUNCTION to_before_alloc
END PROGRAM test_target_mapping_before_alloc
