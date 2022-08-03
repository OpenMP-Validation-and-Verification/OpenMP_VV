!===--- test_requires_reverse_offload.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This test checks to see that the reverse_offload clause on a requires directive 
! is supported, and if so, the function host_function() will made available as a 
! procedure only on the host. By specificying the 'ancestor' modifier with device 
! number of 1, we are indicating to compiler that execution is to be performed on
! the immediate parent, the host. The omp declare target statement ensures that the
! host_function will only be available on host.  
!
! Based on OpenMP 5.0 Example: target_reverse_offload.7.c
! 
!//===----------------------------------------------------------------------===//
#define OMPVV_MODULE_REQUIRES_LINE !$omp requires reverse_offload
#include "ompvv.F90"

#define N 1024

PROGRAM test_requires_reverse_offload
  !$omp requires reverse_offload
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  INTEGER, DIMENSION(N) :: A
  LOGICAL :: isOffloading
  INTEGER :: device_num, i
  LOGICAL :: is_shared_env

  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading)

  OMPVV_WARNING_IF(.not. isOffloading, "Without offloading enabled, host execution is already guaranteed")

  device_num = omp_get_num_devices()

  DO i = 1, N
     A(i) = i
  END DO

  OMPVV_WARNING_IF(device_num .le. 0, "Cannot properly properly test reverse offload if no devices are available")
  OMPVV_TEST_AND_SET_SHARED_ENVIRONMENT(is_shared_env)
  OMPVV_WARNING_IF(is_shared_env, "[WARNING] May not be able to detect errors if the target system supports shared memory.")

  !$omp target enter data map(to: A)

  IF (device_num .gt. 0) THEN
    !$omp target device(ancestor: 1) map(always, to: A)
    DO i = 1, N
       A(i) = 2*i
    END DO
    !$omp end target
  END IF

  DO i = 1, N
     OMPVV_TEST(A(i) .ne. 2*i)
  END DO

  OMPVV_REPORT_AND_RETURN()
END PROGRAM test_requires_reverse_offload
