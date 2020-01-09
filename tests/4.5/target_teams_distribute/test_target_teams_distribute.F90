!===--- test_target_teams_distribute.F90 -----------------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the target teams distribute directive and tests to validate
! that computation inside the region executes properly.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_teams_distribute
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_SHARED_ENVIRONMENT

  OMPVV_TEST_VERBOSE(test_distribute() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_distribute()
    INTEGER,DIMENSION(N):: num_teams, a, b
    INTEGER:: errors, x
    errors = 0

    DO x = 1, N
       a(x) = 1
       b(x) = x
       num_teams(x) = -1
    END DO

    !$omp target data map(tofrom: a(1:N), num_teams(1:N)) map(to: b(1:N))
    !$omp target teams distribute map(alloc: a(1:N), num_teams(1:N), b(1:N))
    DO x = 1, N
       num_teams(x) = omp_get_num_teams();
       a(x) = a(x) + b(x)
    END DO
    !$omp end target data

    IF (num_teams(1) .eq. 1) THEN
       OMPVV_WARNING("Test ran with one team, can't guarantee parallelism of teams")
    ELSE IF (num_teams(1) .lt. 1) THEN
       OMPVV_ERROR("omp_get_num_teams() reported a value below one")
       errors = errors + 1
    END IF

    DO x = 2, N
       IF (num_teams(x) .ne. num_teams(x - 1)) THEN
          OMPVV_ERROR("Test reported an inconsistent number of teams")
          errors = errors + 1
       END IF
       OMPVV_TEST_AND_SET_VERBOSE(errors, a(x) .ne. 1 + b(x))
       IF (a(x) .ne. 1 + b(x)) THEN
          errors = errors + 1
          exit
       END IF
    END DO

    test_distribute = errors
  END FUNCTION test_distribute
END PROGRAM test_target_teams_distribute
