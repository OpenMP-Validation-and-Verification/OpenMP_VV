!===--- test_target_teams_distribute_default_private.F90--------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the default firstprivate clause on a target teams distribute
! directive to indicate that a scalar variable should be by default made
! firstprivate to each team executing the teams distribute region.  The test
! then operates on the firstprivatized variable in such a way that would most
! likely cause competiting operations if the variable is not privatized, and
! such that the outcome depends on the initial host value of the variable. If
! the computation completes without errors, we assume that the
! firstprivatization occured.
!
!===------------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_target_teams_distribute_device
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  OMPVV_TEST_OFFLOADING
  OMPVV_TEST_VERBOSE(test_default_firstprivate() .ne. 0)

  OMPVV_REPORT_AND_RETURN()
CONTAINS
  INTEGER FUNCTION test_default_firstprivate()
    INTEGER,DIMENSION(N):: a, b, c, d
    INTEGER:: privatized, errors, x, y
    errors = 0

    DO x = 1, N
       a(x) = 1
       b(x) = x
       c(x) = 2*x
       d(x) = 0
    END DO

    privatized = 5

    !$omp target teams distribute default(firstprivate) shared(a, &
    !$omp& b, c, d)
    DO x = 1, N
       DO y = 1, a(x) + b(x)
          privatized = privatized + 1
       END DO
       d(x) = c(x)*privatized
    END DO

    DO x = 1, N
       OMPVV_TEST_AND_SET(errors, (d(x) .ne. (2*x)*(6 + x)))
    END DO

    test_default_firstprivate = errors
  END FUNCTION test_default_firstprivate
END PROGRAM test_target_teams_distribute_device
