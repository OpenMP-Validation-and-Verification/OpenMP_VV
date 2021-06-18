!/===--- test_target_uses_allocators_const.F90 ----------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! The tests checks the uses_allocators clause with omp_const_mem_alloc.
! The variable allaocated in the target is modified. Result is copied back to
! the host and checked with computed value on host.
!
!/===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_uses_allocators_const

  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  OMPVV_TEST_VERBOSE(test_target() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_target()
    INTEGER:: x, errors, device_result, host_result, i, j
    errors = 0
    x = 0
    device_result = 0
    host_result = 0

    DO i = 1, N
       DO j = 1, N
          host_result = host_result + j + i
       END DO
    END DO

    !$omp target uses_allocators(omp_const_mem_alloc) allocate(omp_const_mem_alloc: x) firstprivate(x) map(from: device_result)
    DO i = 1, N
       DO j = 1, N
          x = x + j + i
       END DO
    END DO
    device_result = x
    !$omp end target

    OMPVV_TEST_AND_SET_VERBOSE(errors, host_result .ne. device_result)

    test_target = errors
  END FUNCTION test_target
END PROGRAM test_uses_allocators_const
