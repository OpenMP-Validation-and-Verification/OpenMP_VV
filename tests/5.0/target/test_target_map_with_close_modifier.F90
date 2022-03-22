!===--- test_target_map_with_close_modifier.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This test checks for support of the close map-type-modifier on a map clause. The test maps several
! different data types to device with tofrom map-type and then checks for expected updated values on
! the host.
! 
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_map_with_close_modifier
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(test_close_modifier() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_close_modifier()
    INTEGER, DIMENSION(N) :: a
    INTEGER :: errors, i, scalar

    TYPE memberT
    INTEGER :: var
    INTEGER, DIMENSION(N) :: b
    END TYPE memberT

    TYPE(memberT) :: member

    scalar = 19
    member%var = 1

    errors = 0

    DO i = 1, N
       a(i) = i
       member%b(i) = i
    END DO

    !$omp target map(close,tofrom : scalar, a, member)
    scalar = scalar + 25
    member%var = member%var + 16
    DO i = 1, N
       a(i) = a(i) + i * 2
       member%b(i) = member%b(i) + i * 2
    END DO
    !$omp end target

    DO i = 1, N
       OMPVV_TEST_AND_SET(errors, a(i) .ne. i * 3)
       OMPVV_TEST_AND_SET(errors, member%b(i) .ne. i * 3)
    END DO

    OMPVV_TEST_AND_SET(errors, scalar .ne. 44)
    OMPVV_TEST_AND_SET(errors, member%var .ne. 17)

    test_close_modifier = errors
  END FUNCTION test_close_modifier
END PROGRAM test_target_map_with_close_modifier
