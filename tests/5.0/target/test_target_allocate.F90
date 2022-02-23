!===--- test_target_allocate.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! Tests the target directive with allocate clause.
! 
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_allocate
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(target_allocate() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION target_allocate()
    INTEGER :: errors, i
    INTEGER :: x, host_result, device_result

    errors = 0
    x = 0
    host_result = 0
    device_result = 0

    DO i = 1, N
       host_result = host_result + i
    END DO


    !$omp target allocate(omp_default_mem_alloc:x) firstprivate(x) map(from: device_result)
    DO i = 1, N
       x = x + i
    END DO
    device_result = x
    !$omp end target

    OMPVV_TEST_AND_SET(errors, device_result /= host_result)

    target_allocate = errors
  END FUNCTION target_allocate
END PROGRAM test_target_allocate
