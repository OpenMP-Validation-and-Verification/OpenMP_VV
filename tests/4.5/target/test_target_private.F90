!===--test_target_private.F90 - private clause in the target construct --===!
!
! OpenMP API Version 4.5 Nov 2015
!
! Testing the private functionality when used in the target construct
!
!!===----------------------------------------------------------------------===!
#include "ompvv.F90"

#define N 1000

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
    INTEGER :: compute_array(N,OMPVV_NUM_THREADS_DEVICE)
    INTEGER :: actualThreadCnt = 0
    INTEGER :: errors, i, j, p_val, fp_val
    CHARACTER(len=400) :: messageHelper

    compute_array(:,:) = 0
    errors = 0

    CALL omp_set_num_threads(OMPVV_NUM_THREADS_DEVICE)

    !$omp parallel private(p_val, fp_val) shared(actualThreadCnt)
    fp_val = omp_get_thread_num() + 1
    IF (omp_get_thread_num() == 0) THEN
       actualThreadCnt = omp_get_num_threads()
    END IF
    !$omp target map(tofrom:compute_array(:,fp_val)) map(to:fp_val) private(p_val)
    p_val = fp_val
    compute_array(:,p_val) = p_val
    !$omp end target
    !$omp end parallel

    WRITE(messageHelper, '(A,I0,A,I0)') "Test ran with ", &
         actualThreadCnt, " threads out of ", omp_get_thread_limit()
    OMPVV_INFOMSG(messageHelper)

    WRITE(messageHelper, *) "The number of threads in the &
         & host is 1. Test is inconclusive"
    OMPVV_WARNING_IF(actualThreadCnt <= 1, messageHelper)

    DO j=1,actualThreadCnt
       DO i=1, N
          OMPVV_TEST_AND_SET_VERBOSE(errors, compute_array(i,j) .ne. j)
       END DO
    END DO

    test_target_private_clause = errors

  END FUNCTION test_target_private_clause
END PROGRAM test_target_private
