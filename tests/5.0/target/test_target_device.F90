!===--- test_target_device.F90 -------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This test checks the target construct with device clause where device-
! modifier is either ancestor or device_num. If no device_modifier is 
! present, the behavior is the same as if device_num were present.
! 
!//===----------------------------------------------------------------------===//
#define OMPVV_MODULE_REQUIRES_LINE !$omp requires reverse_offload
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_device
  ! Required for device(ancestor: 1)
  !$omp requires reverse_offload
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(target_device_ancestor() .ne. 0)
  OMPVV_TEST_VERBOSE(target_device_device_num() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION target_device_ancestor()
    INTEGER :: errors, i, which_device
    INTEGER, DIMENSION(N) :: a
    LOGICAL :: is_shared_env

    errors = 0
    which_device = 0
    is_shared_env = .false.

    DO i = 1, N
       a(i) = i
    END DO

    OMPVV_TEST_AND_SET(errors, omp_get_num_devices() .le. 0) 
    OMPVV_WARNING_IF(omp_get_num_devices() .le. 0, "[SKIPPED] Since no target devices were found, this test will be skipped.")
    OMPVV_TEST_AND_SET_SHARED_ENVIRONMENT(is_shared_env)
    OMPVV_WARNING_IF(is_shared_env, "[WARNING] target_device_ancestor() test may not be able to detect errors if the target system supports shared memory.")


  !$omp target
    !$omp target device(ancestor: 1) map(tofrom: a) map(to: which_device)
    DO i = 1, N
       a(i) = a(i) + 2
    END DO
    which_device = 75
    !$omp end target

    OMPVV_ERROR_IF(which_device /= 75, "Target region was executed on a target device. Due to ancestor device-modifier this region should execute on a host device")

    END IF


    target_device_ancestor = errors
  END FUNCTION target_device_ancestor

  INTEGER FUNCTION target_device_device_num()
    INTEGER :: errors, i, target_device_num, host_device_num, first_device_num
    INTEGER, DIMENSION(N) :: b

    errors = 0

    DO i = 1, N
       b(i) = i
    END DO

    host_device_num = omp_get_device_num()
    target_device_num = host_device_num

    OMPVV_TEST_AND_SET(errors, omp_get_num_devices() .le. 0) 
    OMPVV_WARNING_IF(omp_get_num_devices() .le. 0, "[SKIPPED] Since no target devices were found, this test will be skipped.")

    IF ( omp_get_num_devices() .gt. 0 ) THEN

    first_device_num = omp_get_num_devices() - 1
    !$omp target device(device_num: first_device_num) map(tofrom: b, target_device_num)
    DO i = 1, N
       b(i) = b(i) + 2
    END DO
    target_device_num = omp_get_device_num()
    !$omp end target

    OMPVV_ERROR_IF(target_device_num .eq. host_device_num, "Target region was executed on host, this region should execute on specified target device number")

    END IF

    OMPVV_TEST_AND_SET(errors, target_device_num .eq. host_device_num)

    target_device_device_num = errors
  END FUNCTION target_device_device_num
END PROGRAM test_target_device
