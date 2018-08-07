!===---- test_target_device.F90 -  -----------------------------------===//
! 
! OpenMP API Version 4.5 Nov 2015
!
! Testing the use of the device clause in the target construct
!
!===----------------------------------------------------------------------===//
#include "ompvv.F90" 

#define N 1000

      PROGRAM test_target_device
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        
        OMPVV_TEST_OFFLOADING
        OMPVV_TEST_VERBOSE(test_target_device_clause() /= 0)
        OMPVV_REPORT_AND_RETURN()

        CONTAINS 
          INTEGER FUNCTION test_target_device_clause()
            INTEGER:: i, dev
            INTEGER:: num_dev
            INTEGER:: array(N)
            CHARACTER(len=100):: message

            OMPVV_INFOMSG("test_target_device_clause")
            ! Get number of devices
            num_dev = omp_get_num_devices()
            WRITE(message, '(A,I0)') "num_devices tested = ", num_dev
            OMPVV_INFOMSG(message)
            
            ! Array initialization
            array(:) = -1
          
            ! Map the same array to multiple devices. initialize with device number
            DO dev = 0, num_dev - 1
              !$omp target map(tofrom: array(1:N)) device(dev)
                    array(:) = array(:) + dev + 1
              !$omp end target
              
              ! checking for results
              OMPVV_TEST_VERBOSE(ANY(array /= dev, 1))
              ! host array re-initialization
              array(:) = -1
            END DO 

            OMPVV_GET_ERRORS(test_target_device_clause)
          END FUNCTION test_target_device_clause
      END PROGRAM test_target_device
