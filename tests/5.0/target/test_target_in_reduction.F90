!===--- test_target_in_reduction.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This test checks task reductions for a target task resulting from a target
! construct with the 'in_reduction' clause.
! 
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1028

PROGRAM test_target_in_reduction
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  IMPLICIT NONE 
  INTEGER :: i, host_reduction_sum, device_reduction_sum
  INTEGER :: sum, total, errors

  host_reduction_sum = 0
  device_reduction_sum = 0
  sum = 0
  total = 0
  errors = 0 

  !$omp parallel master
  !$omp taskgroup task_reduction(+:sum)
  !$omp target in_reduction(+:sum)
  CALL compute_on_device(sum)
  !$omp end target
  !$omp task in_reduction(+:sum)
  CALL compute_on_host(sum)
  !$omp end task
  !$omp end taskgroup
  !$omp end parallel master 
  
  DO i = 1, N
    device_reduction_sum = device_reduction_sum + 2
    host_reduction_sum = host_reduction_sum + 1
  END DO

  OMPVV_TEST_VERBOSE(sum /= (device_reduction_sum + host_reduction_sum))

  OMPVV_TEST_VERBOSE(sum == device_reduction_sum)
  OMPVV_ERROR_IF(sum == device_reduction_sum, "Host task did not participate in the reduction")

  OMPVV_TEST_VERBOSE(sum == host_reduction_sum)
  OMPVV_ERROR_IF(sum == host_reduction_sum, "Target task did not participate in the reduction")

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  SUBROUTINE compute_on_device(sum)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: sum
    INTEGER :: i


    DO i = 1, N
       sum = sum + 2
    END DO
  END SUBROUTINE compute_on_device

  SUBROUTINE compute_on_host(sum)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: sum
    INTEGER :: i


    DO i = 1, N
       sum = sum + 1
    END DO
  END SUBROUTINE compute_on_host
END PROGRAM test_target_in_reduction
