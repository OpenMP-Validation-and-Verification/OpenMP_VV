!===--test_target_map_array_default.F90 - test default behavior of array map--===!
! 
! OpenMP API Version 4.5 Nov 2015
!
! Whenever a map-type-modifier is not specified in the map clause, the symbol
! is mapped as a tofrom. This test make sure this is satisfied
!
!!===----------------------------------------------------------------------===!
#include "ompvv.F90"

#define N 1000

      PROGRAM test_target_map_array_default
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        
        OMPVV_TEST_OFFLOADING
        OMPVV_TEST_VERBOSE(test_array_map_no_map_type() .ne. 0)

        OMPVV_REPORT_AND_RETURN()


        CONTAINS 
          INTEGER FUNCTION test_array_map_no_map_type ()
            INTEGER :: compute_array(N)
            INTEGER :: i
 
            compute_array(:) = 10

            !$omp target map(compute_array) 
              compute_array(:) =  (/ (compute_array(i) + i, i = 1,N) /)
            !$omp end target

            OMPVV_TEST_VERBOSE(SUM(compute_array) /= ((N*(N+1)/2) + 10*N))

            OMPVV_GET_ERRORS(test_array_map_no_map_type)

          END FUNCTION test_array_map_no_map_type
      END PROGRAM test_target_map_array_default

