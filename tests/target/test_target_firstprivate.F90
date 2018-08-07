!===---- test_target_firstprivate.F90 -  -----------------------------------===//
! 
! OpenMP API Version 4.5 Nov 2015
!
! test for the firstprivate clause used inside the target construct. This clause
! should copy the original result of the value over to the target device, and
! create a private storage location per thread
!
!===----------------------------------------------------------------------===//
#include "ompvv.F90" 

#define N 10
#define NUM_THREADS_TEST 10
      PROGRAM test_target_firstprivate
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        
        OMPVV_TEST_OFFLOADING
        OMPVV_TEST_VERBOSE(test_target_firstprivate_clause() .ne. 0)

        OMPVV_REPORT_AND_RETURN()


        CONTAINS 
          INTEGER FUNCTION test_target_firstprivate_clause()
            INTEGER :: compute_array(N,NUM_THREADS_TEST)
            INTEGER :: p_val
            INTEGER :: actualThreadCnt = 0
            CHARACTER(len=400) :: messageHelper

            OMPVV_INFOMSG("Testing firstprivate clause with target")
            
            compute_array(:,:) = 0

            call omp_set_num_threads(NUM_THREADS_TEST)
            !$omp parallel private(p_val) shared(actualThreadCnt)
              ! each p_val is private to a thread
              p_val = omp_get_thread_num() + 1
              actualThreadCnt = omp_get_num_threads()
              !$omp target map(tofrom:compute_array(:, p_val)) firstprivate(p_val)
                ! the p_val should be private and initialized to the p_val
                ! before the target region
                compute_array(:,p_val) = 100
                ! Testing the privatization
                p_val = p_val + 1
              !$omp end target
              ! Testing that the target region did not modify the original value
              ! of p_val
              IF (p_val == omp_get_thread_num() + 1) THEN
                compute_array(:, p_val) = compute_array(:, p_val) + 1
              END IF
            !$omp end parallel 

            WRITE(messageHelper, '(A,I0,A,I0)') "Test ran with ", &
            actualThreadCnt, " threads out of ", omp_get_thread_limit()
            OMPVV_INFOMSG(messageHelper)

            WRITE(messageHelper, *) "The number of threads in the &
            & host is 1. Test is inconclusive"
            OMPVV_WARNING_IF(actualThreadCnt <= 1, messageHelper)

            OMPVV_TEST_VERBOSE(ANY(compute_array(:,1:actualThreadCnt) /= 101))

            OMPVV_GET_ERRORS(test_target_firstprivate_clause)
          END FUNCTION test_target_firstprivate_clause
      END PROGRAM test_target_firstprivate


