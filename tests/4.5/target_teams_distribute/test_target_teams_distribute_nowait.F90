!===--- test_target_teams_distribute_nowait.F90-----------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the nowait clause on a target teams distribute directive and
! uses a barrier to resyncronize the target regions.  Since we can't be sure
! that operations will be asyncronous, we can not test to make sure that
! the regions are executed asynchronously.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024
#define N_TASKS 16
#define ITERATIONS 1024

PROGRAM test_target_teams_distribute_nowait
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  INTEGER :: errors
  OMPVV_TEST_OFFLOADING
  errors = 0

  OMPVV_WARNING("This test does not throw an error if tasks fail to execute asynchronously, as this is still correct behavior. If execution is not asynchronous, we will throw a warning.")

  OMPVV_TEST_VERBOSE(test_nowait() .ne. 0)

  OMPVV_REPORT_AND_RETURN()
CONTAINS
  INTEGER FUNCTION test_nowait()
    INTEGER:: errors, x, y, race_condition_found, task
    INTEGER,DIMENSION(N):: a, b, c, d, e, f

    OMPVV_ERROR_IF(MOD(N, N_TASKS) .ne. 0, "Selected value of N not divisible by number of tasks")

    DO y = 1, ITERATIONS
       DO x = 1, N
          a(x) = x + y
          b(x) = 2*x + y
          c(x) = 0
          d(x) = 3*x + y
          e(x) = 4*x + y
          f(x) = 0
       END DO


       DO task = 1, N_TASKS
          !$omp target teams distribute nowait map(to: a(1:N), b(1:N), d(1:N), e(1:N)) &
          !$omp& map(from: c(1:N), f(1:N))
          DO x = 1 + (N / N_TASKS)*(task - 1), (N / N_TASKS)*task
             c(x) = a(x) + b(x)
          END DO
          !$omp end target teams distribute
       END DO
       !$omp target teams distribute map(to: a(1:N), b(1:N), d(1:N), e(1:N)) &
       !$omp& map(from: c(1:N), f(1:N))
       DO x = 1, N
          f(x) = c(x) + d(x) + e(x)
       END DO
       !$omp end target teams distribute
       !$omp taskwait

       DO x = 1, N
          IF (c(x) .ne. 3*x + 2*y) THEN
             errors = errors + 1
          END IF
          IF (f(x) .ne. 10*x + 4*y) THEN
             race_condition_found = 1
          END IF
       END DO
    END DO

    OMPVV_WARNING_IF(race_condition_found == 0, "Could not show that nowait had an effect on target teams distribute construct.")
    OMPVV_INFOMSG_IF(race_condition_found == 1, "At least one race condition was introduced, nowait had an effect.")

    test_nowait = errors
  END FUNCTION test_nowait
END PROGRAM test_target_teams_distribute_nowait
