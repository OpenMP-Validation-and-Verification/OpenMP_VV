!===-----test_loop_iterator.F90--------------------------------------------===//
! OpenMP API Version 5.0 Nov 2021
! Pg. 270, line 21
! *************************
! DIRECTIVE: loop 
! CLAUSES:
! *************************
! This tests checks the lastprivate behavior of the loop iterator variable
! as desrcribed in section 5.1.1 "Variables Referenced in a Construct."
! The test will pass if the integer i has lastprivate data properties. In 
! other words, the updated value of i remains after the loop.
!===----------------------------------------------------------------------===//
#include "ompvv.F90" 

#define N 100

PROGRAM test_loop
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  OMPVV_TEST_OFFLOADING
  OMPVV_TEST_VERBOSE(test_loop_iterator() .NE. 0)
  OMPVV_REPORT_AND_RETURN()

CONTAINS 
  INTEGER FUNCTION test_loop_iterator()
  INTEGER :: errors = 0
  REAL :: a(N)
  INTEGER :: i, num_threads = 0

  !$omp parallel
  !$omp single
    num_threads = omp_get_num_threads()
  !$omp end single

  !$omp target map(a, i)
  !$omp loop
  DO i = 1, N
    a(i) = i
  END DO
  !$omp end loop
  !$omp end target
  !$omp end parallel

  OMPVV_TEST_AND_SET(errors, i .NE. N+1)
  test_loop_iterator = errors

  END FUNCTION test_loop_iterator
END PROGRAM test_loop
