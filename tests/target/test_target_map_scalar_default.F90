! ===--- test_target_map_escalar_default.F90 - testing default map to scalar ===//
! 
! OpenMP API Version 4.5 Nov 2015
! 
! When no map-type-modifier (e.g. to, from and tofrom) are not specified, the 
! default behavior should be tofrom. This test check if this is sattisfied with
! a simple integer value. An array is created an initialized to zero in the host
! then changed in the device with a scalar value.
!
!//===----------------------------------------------------------------------===//
#include "ompvv.F90" 

#define N 1000

      PROGRAM test_target_scalar_defaultmap
        USE iso_fortran_env
        USE ompvv_lib
        implicit none
        
        OMPVV_TEST_OFFLOADING
        OMPVV_TEST_VERBOSE(test_target_defaultmod_map_scalar() /= 0)
        OMPVV_REPORT_AND_RETURN()

        CONTAINS 
          INTEGER FUNCTION test_target_defaultmod_map_scalar()
            INTEGER:: compute_array(N)
            INTEGER:: sclrVal, array_sum, i
            sclrVal = 12
          
            ! Array initialization
            compute_array(:) = 0
          
            !$omp target map(from: compute_array) map(sclrVal)
              compute_array(:) = (/ (i + sclrVal, i = 1, N) /) 
              sclrVal = sclrVal - 10
            !$omp end target
            
            OMPVV_TEST_VERBOSE(sclrVal /= 2)
            OMPVV_TEST_VERBOSE(SUM(compute_array) /= (N*(N+1)/2 + 12*N))

            OMPVV_GET_ERRORS(test_target_defaultmod_map_scalar)

          END FUNCTION test_target_defaultmod_map_scalar
      END PROGRAM test_target_scalar_defaultmap
