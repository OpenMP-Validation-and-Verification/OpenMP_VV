!/===--- test_target_uses_allocators_thread.F90 ---------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! The tests checks the uses_allocators clause with omp_thread_mem_alloc.
! The variable allaocated in the target region is modified and used to compute
! result in device envioronment. Result is copied back to the host and checked
! with computed value on host.
!
!/===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_uses_allocators_thread

  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  OMPVV_TEST_VERBOSE(test_target() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_target()
    INTEGER,DIMENSION(N):: device_result, host_result
    INTEGER:: x, errors, i
    errors = 0
    x = 0

    DO i = 1, N
       host_result(i) = 3*i
       device_result(i) = 0
    END DO

    !$omp target teams distribute uses_allocators(omp_thread_mem_alloc) allocate(omp_thread_mem_alloc: x) private(x) map(from: device_result)
    DO i = 1, N
       x = 2*i
       device_result(i) = i + x
    END DO

    DO i = 1, N
       OMPVV_TEST_AND_SET_VERBOSE(errors, host_result(i) .ne. device_result(i))
    END DO

    test_target = errors
  END FUNCTION test_target
END PROGRAM test_uses_allocators_thread
