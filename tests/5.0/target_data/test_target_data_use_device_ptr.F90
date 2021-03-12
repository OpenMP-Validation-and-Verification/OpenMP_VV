!/===-- test_target_data_use_device_ptr.c ---------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This file is a test for the use_device_ptr when used with the map clause
! with target data directive. This test uses arrays of size N whose values
! are modified on the device and tested in the host. Once the array has
! been mapped to the device, the use_device_ptr should be able to be used
! with the ptr to the array and subsequently modify values on the device.
! This test ensures address conversions of use_device_ptr clauses will
! occur as if performed after all variables are mapped according to those
! map clauses.
!
!/===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_data_use_device_ptr
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(use_device_ptr() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION use_device_ptr()
    INTEGER :: errors, x
    INTEGER,POINTER,DIMENSION(:) :: array_device, array_host

    allocate(array_device(N))
    allocate(array_host(N))

    errors = 0

    DO x = 1, N
       array_device(x) = x
       array_host(x) = 0
    END DO

    !$omp target data map(to: array_device(1:N)) use_device_ptr(array_device)
    !$omp target is_device_ptr(array_device) map(tofrom: array_host(1:N))
    DO x = 1, N
       array_host(x) = array_host(x) + array_device(x)
    END DO
    !$omp end target
    !$omp end target data

    DO x = 1, N
       OMPVV_TEST_AND_SET_VERBOSE(errors, array_host(x) .ne. x)
    END DO

    deallocate(array_device)
    deallocate(array_host)

    use_device_ptr = errors
  END FUNCTION use_device_ptr
END PROGRAM test_target_data_use_device_ptr
