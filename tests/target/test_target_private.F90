!===--test_target_private.F90 - private clause in the target construct --===!
! 
! OpenMP API Version 4.5 Nov 2015
!
! Testing the private functionality when used in the target construct
!
!!===----------------------------------------------------------------------===!
#include "ompvv.F90"

#define N 1000
#define NUM_THREADS 10

      PROGRAM test_target_private
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        
        OMPVV_TEST_OFFLOADING
        OMPVV_TEST_VERBOSE(test_target_private_clause() .ne. 0)

        OMPVV_REPORT_AND_RETURN()

        CONTAINS 
          INTEGER FUNCTION test_target_private_clause()
            INTEGER :: compute_array(NUM_THREADS, N)
            INTEGER :: actualThreadCnt = 0
            INTEGER :: p_val, fp_val
            CHARACTER(len=400) :: messageHelper
           
            compute_array(:,:) = 0
          
            CALL omp_set_num_threads(NUM_THREADS)

            !$omp parallel private(p_val, fp_val) shared(actualThreadCnt)
              fp_val = omp_get_thread_num() + 2
              p_val = omp_get_thread_num() + 1
              actualThreadCnt = omp_get_num_threads()
              !$omp target map(tofrom:compute_array) map(to:fp_val) private(p_val)
                p_val = fp_val - 1
                compute_array(p_val,:) = 100
                p_val = p_val + 99
              !$omp end target
              IF (p_val == omp_get_thread_num() + 1) THEN
                compute_array(p_val,:) = compute_array(p_val,:) + 1
              END IF
            !$omp end parallel 
          
            WRITE(messageHelper, '(A,I0,A,I0)') "Test ran with ", &
            actualThreadCnt, " threads out of ", omp_get_thread_limit()
            OMPVV_INFOMSG(messageHelper)

            WRITE(messageHelper, *) "The number of threads in the &
            & host is 1. Test is inconclusive"
            OMPVV_WARNING_IF(actualThreadCnt <= 1, messageHelper)

            OMPVV_TEST_VERBOSE(ANY(compute_array(:,1:actualThreadCnt) /= 101))

            OMPVV_GET_ERRORS(test_target_private_clause)

          END FUNCTION test_target_private_clause
      END PROGRAM test_target_private

