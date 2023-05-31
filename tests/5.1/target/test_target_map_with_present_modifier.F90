!===--- test_target_map_with_present_modifier.F90 ---------------------------------------------===//
!
! OpenMP API Version 5.1 Nov 2020
!
! This test checks tests the present map-type-modifier on a map clause. The test maps several
! different data types to device with tofrom map-type and then checks for expected updated values on
! the host.
!
!//===-----------------------------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_map_with_present_modifier
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(test_present_modifier() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_present_modifier()
    INTEGER :: errors, i
    INTEGER :: scalar
    INTEGER, DIMENSION(N) :: a

    TYPE memberType
    INTEGER :: var
    INTEGER, DIMENSION(N) :: b
    END TYPE memberType

    TYPE (memberType) :: member

    errors = 0
    scalar = 1
    member%var = 1

    DO i=1, N
      a(i) = i
      member%b(i) = i
    END DO

    !$omp target data map(tofrom: scalar, a, member)
    !$omp target map(present, tofrom: scalar, a, member)
    scalar = scalar + 1
    member%var = member%var + 2
    DO i=1, N
      a(i) = a(i) + i
      member%b(i) = member%b(i) + i
    END DO
    !$omp end target
    !$omp end target data

    DO i=1, N
      OMPVV_TEST_AND_SET(errors, a(i) .NE. i*2)
      OMPVV_TEST_AND_SET(errors, member%b(i) .NE. i*2)
    END DO

    OMPVV_TEST_AND_SET(errors, scalar .NE. 2)
    OMPVV_TEST_AND_SET(errors, member%var .NE. 3)

    test_present_modifier = errors
  END FUNCTION test_present_modifier
END PROGRAM test_target_map_with_present_modifier

