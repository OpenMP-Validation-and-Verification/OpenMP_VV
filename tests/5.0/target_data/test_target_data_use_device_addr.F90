!===--- test_target_data_use_device_addr.F90 ------------------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This file is a test for the use_device_addr when used with the map clause with
! target data directive. This test uses a scalar and an array of size N which values
! are modified on the  device and tested in the host. List items that  appear in a
! use_device_addr clause have the address of the corresponding object in the device
! data environment inside the construct. This test also tests that address conversions
! of use_device_addr clauses will occur as if performed after all variables are mapped
! according to those map clauses.
!
!===-----------------------------------------------------------------------------------===//

#include "ompvv.F90"

PROGRAM test_target_data_use_device_addr
   USE iso_fortran_env
   USE ompvv_lib
   USE omp_lib
   implicit none

   REAL, POINTER :: dev_ptr
   INTEGER :: errors, host_data, device_data

   errors = 0
   device_data = 14
   host_data = 0 

   ALLOCATE(dev_ptr)

   OMPVV_TEST_OFFLOADING

   OMPVV_TEST_VERBOSE(test(dev_ptr) .ne. 0)

   OMPVV_REPORT_AND_RETURN()


CONTAINS
   INTEGER FUNCTION test(x)
      REAL, INTENT(INOUT) :: x

      !$omp target data map(to: device_data)

         !$omp target data use_device_addr(device_data)
            x = device_data
         !$omp end target data

         !$omp target map(to:device_data) map(tofrom: errors) map(from:host_data) is_device_ptr(x)
            IF (device_data .ne. x) THEN
               errors = errors + 1
            END IF
         !$omp end target

         !$omp target map(from: host_data) is_device_ptr(x)
            host_data = x
         !$omp end target

      !$omp end target data

      !checking results
      OMPVV_TEST_AND_SET(errors, host_data .ne. 14)

      test = errors
   END FUNCTION test
END PROGRAM test_target_data_use_device_addr

