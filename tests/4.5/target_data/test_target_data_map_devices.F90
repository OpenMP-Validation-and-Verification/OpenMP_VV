!===---- test_target_data_map_devices.F90 - using target data with multiple dev --===//
! 
! OpenMP API Version 4.5 Nov 2015
! 
! Iterating over the number of devices, this tests uses target data with
! multiple devices using the devices clause !
! 
!===-------------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 5000

      PROGRAM test_target_data_devices
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        LOGICAL :: isOffloading, isSharedEnv
        INTEGER :: i, j, num_dev
        CHARACTER(len=500) :: msgHelper

        OMPVV_TEST_AND_SET_OFFLOADING(isOffloading)
        OMPVV_TEST_AND_SET_SHARED_ENVIRONMENT(isSharedEnv)

        ! Reporting the number of devices
        num_dev = omp_get_num_devices()
        WRITE(msgHelper, '(A,I0)') "number of devices = ", num_dev
        OMPVV_INFOMSG(msgHelper)

        ! Warning if only one device
        WRITE(msgHelper, *) "The number of devices is 1, this test &
        &is inconclussive"
        OMPVV_WARNING_IF(num_dev == 1, msgHelper)

        !starting the tests
        OMPVV_TEST_VERBOSE(test_device_clause() .NE. 0)

        OMPVV_REPORT_AND_RETURN()

        CONTAINS 
          ! Testing set device with deviceclause
          INTEGER FUNCTION test_device_clause()
          INTEGER :: dev_data, dev_comp
          INTEGER, dimension(N) :: anArray

          OMPVV_INFOMSG("using device() clause")

            ! Initialize the array
            anArray(:) = 1

            ! Iterate over all the devices
            DO dev_data = 0, num_dev - 1
              !$omp target data map(tofrom: anArray(1:N)) &
              !$omp device(dev_data)

              ! Iterate over all the devices doing comp
              ! to guarantee that the right data env is 
              ! being copied back and forth
                DO dev_comp = 0, num_dev - 1
                  !$omp target map(alloc: anArray(1:N)) &
                  !$omp device(dev_comp)
                    ! Increase only in the current tested device
                    IF (dev_comp == dev_data) THEN
                      anArray(1:N) = anArray(1:N) + 1
                    ELSE IF (.NOT. isSharedEnv) THEN
                      ! This should not affect result as it should happen in
                      ! other devices
                      anArray(1:N) = anArray(1:N) - 10
                    END IF
                  !$omp end target
                END DO ! dev_comp
             !$omp end target data
            END DO ! dev_data
              
            OMPVV_TEST_VERBOSE(ANY(anArray /= num_dev + 1))

            OMPVV_GET_ERRORS(test_device_clause)

          END FUNCTION test_device_clause
      END PROGRAM test_target_data_devices

