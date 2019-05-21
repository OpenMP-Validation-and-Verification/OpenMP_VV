!===--- test_target_map_pointer.c - test target with map pointer p[:N] -----===//
! 
! OpenMP API Version 4.5 Nov 2015
!
! This test check if it is possible to map an array and a pointer to that array,
! and then access the array through the pointer. It is necessary
! to specify the array size with [:N]. If offloading is used, the value of p[] 
! is copied over the device. The array will be updated inside de omp target
! region and compared afterwards
!
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1000

      PROGRAM test_target_map_pointer
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        
        OMPVV_TEST_OFFLOADING
        OMPVV_TEST_VERBOSE(test_target_map_clause_pointer() .ne. 0)

        OMPVV_REPORT_AND_RETURN()


        CONTAINS 
          INTEGER FUNCTION test_target_map_clause_pointer()
            INTEGER, TARGET, DIMENSION(N) :: compute_array
            INTEGER, POINTER, DIMENSION(:) :: ptr

            ! Initialize array and pointer
            compute_array(:) = 0
            ptr => compute_array
            !$omp target 
              ptr(:) = 10
            !$omp end target

            OMPVV_TEST_VERBOSE(ANY(compute_array /= 10))

            OMPVV_GET_ERRORS(test_target_map_clause_pointer)

          END FUNCTION test_target_map_clause_pointer
      END PROGRAM test_target_map_pointer

