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
#define ITERATIONS 1024

PROGRAM test_target_teams_distribute_nowait
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  INTEGER :: errors
  OMPVV_TEST_OFFLOADING
  errors = 0

  OMPVV_WARNING("This test cannot fail at runtime. It can only issue warnings when nowait couldn't be tested.")

  OMPVV_TEST_VERBOSE(test_nowait() .ne. 0)

  OMPVV_REPORT_AND_RETURN()
CONTAINS
  INTEGER FUNCTION test_nowait()
    INTEGER:: errors, x, y, race_condition_found
    INTEGER,DIMENSION(N):: a, b, c, d, e, f

    DO y = 1, ITERATIONS
       DO x = 1, N
          a(x) = x + y
          b(x) = 2*x + y
          c(x) = 0
          d(x) = 3*x + y
          e(x) = 4*x + y
          f(x) = 0
       END DO


       !$omp target data map(to: a(1:N), b(1:N), d(1:N), e(1:N)) map(from: &
       !$omp& c(1:N), f(1:N))
       !$omp target teams distribute nowait map(alloc: a(1:N), b(1:N), &
       !$omp& c(1:N))
       DO x = 1, N
          c(x) = a(x) + b(x)
       END DO
       !$omp end target teams distribute
       !$omp target teams distribute map(alloc: c(1:N), d(1:N), e(1:N), &
       !$omp& f(1:N))
       DO x = 1, N
          f(x) = c(x) + d(x) + e(x)
       END DO
       !$omp end target teams distribute
       !$omp taskwait
       !$omp end target data

       DO x = 1, N
          IF (c(x) .ne. 3*x + 2*y) THEN
             errors = errors + 1
          END IF
          IF (f(x) .ne. 10*x + 4*y) THEN
             race_condition_found = 1
          END IF
       END DO
    END DO

    OMPVV_WARNING_IF(race_condition_found == 0, "Could not show that nowait was operating on target teams distribute construct.")
    OMPVV_INFOMSG_IF(race_condition_found == 1, "At least one race condition was introduced, nowait was operating.")

    test_nowait = errors
  END FUNCTION test_nowait
END PROGRAM test_target_teams_distribute_nowait
