!===--- test_target_imperfect_loop.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! The test maps two arrays to the device and uses the collapse clause on the work 
! sharing loop construct enclosing two loops. According to 5.0 Spec if more than 
! one loop is associated with the worksharing-loop construct then the number of 
! times that any intervening code between any two associated loops will be executed 
! is unspecified but will be at least once per iteration of the loop enclosing the 
! intervening code and at most once per iteration of the innermost loop associated 
! with the construct.The value modified on the device(if oddloaded) and is verified 
! on the host for correctness.
! This test is a modified version of an example and provided by LLNL.  
! 
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 10
#define M 16

PROGRAM test_target_imperfect_loop
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(target_imperfect_loop() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION target_imperfect_loop()
    INTEGER, DIMENSION(N) :: data1
    INTEGER, DIMENSION(N,M) :: data2
    INTEGER :: errors, i, j

    errors = 0

    DO i = 1, N
       data1(i) = 0
       DO j = 1, M
          data2(i,j) = 0
       END DO
    END DO

    !$omp target map(tofrom: data1, data2)
    !$omp parallel do collapse(2)
    DO i = 1, N
       data1(i) = data1(i) + i
       DO j = 1, M
          data2(i,j) = data2(i,j) + i + j
       END DO
    END DO
    !$omp end parallel do
    !$omp end target

    DO i = 1, N
       OMPVV_TEST_AND_SET(errors, data1(i) .lt. i)
       OMPVV_TEST_AND_SET(errors, data1(i) .gt. i * M)
       DO j = 1, M
          OMPVV_TEST_AND_SET(errors, data2(i,j) .ne. i + j)
       END DO
    END DO

    target_imperfect_loop = errors
  END FUNCTION target_imperfect_loop
END PROGRAM test_target_imperfect_loop
