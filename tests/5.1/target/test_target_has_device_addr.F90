!===--- test_target_has_device_addr.F90 ------------------------------------===//
!
! OpenMP API Version 5.1 Nov 2020
!
! This test verifies the 'has_device_addr' feature added to the target construct.
! We tested this by mapping a scalar & array to the device
! and ensuring that the address does not change when using
! has_device_addr on another target region.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_has_device_addr
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(target_has_device_addr() .NE. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION target_has_device_addr()
    INTEGER :: errors, i
    INTEGER, TARGET :: x
    INTEGER, TARGET, DIMENSION(N) :: arr
    INTEGER, POINTER :: first_scalar_device_addr
    INTEGER, POINTER :: first_arr_device_addr(:)
    INTEGER, POINTER :: second_scalar_device_addr
    INTEGER, POINTER :: second_arr_device_addr(:)

    errors = 0
    x = 10
    DO i=1, N
      arr(i) = i
    END DO

    OMPVV_INFOMSG("test_target_has_device_addr")

    ! test by mapping to device use 'target map' construct
    !$omp target enter data map(to: x, arr)
    !$omp target map(from: first_scalar_device_addr, first_arr_device_addr) map(to: x, arr)
    first_scalar_device_addr => x
    first_arr_device_addr => arr
    !$omp end target

    ! check addresses are same on device region
    !$omp target data use_device_addr(x, arr)
    !$omp target map(from: second_scalar_device_addr, second_arr_device_addr) has_device_addr(x, arr)
    second_scalar_device_addr => x
    second_arr_device_addr => arr
    !$omp end target
    !$omp end target data
    !$omp target exit data map(release: x, arr)

    OMPVV_TEST_AND_SET(errors, associated(first_scalar_device_addr, target=second_scalar_device_addr))
    OMPVV_TEST_AND_SET(errors, associated(first_arr_device_addr, target=second_arr_device_addr))

    target_has_device_addr = errors
  END FUNCTION target_has_device_addr
END PROGRAM test_target_has_device_addr

