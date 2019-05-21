!===---- test_target_async.F90 -  -----------------------------------===//
! 
! OpenMP API Version 4.5 Nov 2015
!
! This tests checks the target dependencies as well as the asynchronous
! behavior of the target clause
!
!===----------------------------------------------------------------------===//
#include "ompvv.F90" 

#define N 1024

      PROGRAM test_target_async
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        
        OMPVV_TEST_OFFLOADING

        OMPVV_TEST_VERBOSE(tests_dependencies() /= 0)

        OMPVV_REPORT_AND_RETURN()
        CONTAINS 
          INTEGER FUNCTION tests_dependencies()
            INTEGER :: dep_1(N), dep_2(N)
            CHARACTER(len=400) :: messageHelper
            OMPVV_INFOMSG("test_all_dependencies")
          
            ! Initialize dep_1 and dep_2
            dep_1(:) = 0
            dep_2(:) = 0
          
            ! Map the same array to multiple devices. initialize with device number
            !$omp target depend(out: dep_1) map(tofrom: dep_1(1:N))
              dep_1(:) = 1
            !$omp end target 
          
            !$omp target depend(out: dep_2) map(tofrom: dep_2(1:N))
              dep_2(:) = 1
            !$omp end target 
          
            !$omp task depend(inout: dep_1) depend(inout: dep_2) &
            !$omp shared(dep_1, dep_2)
              dep_1(:) = dep_1(:) + 1
              dep_2(:) = dep_2(:) + 1
            !$omp end task
          
            !$omp target depend(inout: dep_1) depend(inout: dep_2) &
            !$omp map(tofrom: dep_1(1:N), dep_2(1:N))
              dep_1(:) = dep_1(:) + 1
              dep_2(:) = dep_2(:) + 1
            !$omp end target 
          
            !$omp target depend(in: dep_1) depend(in: dep_2) &
            !$omp map(tofrom: dep_1(1:N))  map(tofrom: dep_2(1:N))
              dep_1(:) = dep_1(:) + 1
              dep_2(:) = dep_2(:) + 1
            !$omp end target
          
            !$omp taskwait
            
            OMPVV_TEST_VERBOSE(ANY(dep_1 /= 4))
            OMPVV_TEST_VERBOSE(ANY(dep_2 /= 4))

            OMPVV_GET_ERRORS(tests_dependencies)
          END FUNCTION tests_dependencies
      END PROGRAM test_target_async
