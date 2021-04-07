!===--- test_loop_order_concurrent.F90 -------------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This test checks the loop directive with the order(concurrent) clause.
! The order(concurrent) clause is assumed to be present if it is not
! present, so this test covers the standalone loop directive as well. The
! test creates a parallel region with a loop construct nested within, and
! performs simple operations on an int array which are then checked for
! correctness. Additionally, since loop binds to a parallel region, the test
! checks randomly that other threads wait before proceeding out of the loop
! region. The number of threads is checked in the parallel region but after
! the loop construct because runtime API calls are not permitted in loop
! directive regions.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_loop_order_concurrent
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_SHARED_ENVIRONMENT

  OMPVV_TEST_VERBOSE(test_loop() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_loop()
    CHARACTER(len=300):: infoMsg
    INTEGER,DIMENSION(N):: a, b, c
    INTEGER,DIMENSION(OMPVV_NUM_THREADS_HOST):: rand_indexes
    INTEGER:: errors, x
    REAL:: curr_rand
    errors = 0

    DO x = 1, N
       a(x) = 1
       b(x) = x + 1
       c(x) = 2*(x + 1)
    END DO

    CALL init_random_seed()

    DO x = 1, OMPVV_NUM_THREADS_HOST
       CALL RANDOM_NUMBER(curr_rand)
       rand_indexes(x) = MODULO((curr_rand*10),(N + 1))
    END DO

    !$omp parallel num_threads(OMPVV_NUM_THREADS_HOST)
    !$omp loop order(concurrent)
    DO x = 1, N
       num_teams(x) = omp_get_num_teams();
       a(x) = a(x) + b(x)
    END DO
    !$omp end parallel

    OMPVV_WARNING_IF(num_teams(1) .eq. 1, "Test ran with one team, can't guarantee parallelism of teams")

    OMPVV_TEST_AND_SET_VERBOSE(errors, num_teams(1) .lt. 1)

    IF (errors .eq. 0) THEN
       DO x = 2, N
          OMPVV_TEST_AND_SET_VERBOSE(errors, num_teams(x) .ne. num_teams(x - 1))
          OMPVV_ERROR_IF(num_teams(x) .ne. num_teams(x - 1), "Test reported an inconsistent number of teams")
          OMPVV_TEST_AND_SET_VERBOSE(errors, a(x) .ne. 1 + b(x))
          IF (errors .gt. 0) THEN
             exit
          END IF
       END DO
    END IF

    WRITE(infoMsg, *) "Test passed with", num_teams(1), "teams."
    OMPVV_INFOMSG_IF(errors .eq. 0, infoMsg)

    test_loop = errors
  END FUNCTION test_loop
END PROGRAM test_loop_order_concurrent
