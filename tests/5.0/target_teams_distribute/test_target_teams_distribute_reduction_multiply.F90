!===--- test_target_teams_distribute_reduction_multiply.F90-----------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the reduction clause on a target teams distribute
! directive, testing, for the multiply operator, that the variable in the
! reduction clause is properly reduced.
!
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_teams_distribute_device
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  INTEGER :: errors
  errors = 0

  OMPVV_TEST_OFFLOADING()


  OMPVV_TEST_VERBOSE(test_multiply() .ne. 0)

  OMPVV_REPORT_AND_RETURN()
CONTAINS
  INTEGER FUNCTION test_multiply()
    INTEGER,DIMENSION(N, 8):: a
    INTEGER,DIMENSION(N):: results, host_results
    INTEGER:: x, y, errors

    errors = 0
    host_results = 1
    results = 1

    DO x = 1, N
       DO y = 1, 8
          a(x, y) = MOD(x, 8) + y
       END DO
    END DO

    DO x = 1, N
       DO y = 1, 8
          host_results(x) = a(x, y) * host_results(x)
       END DO
    END DO

    DO x = 1, N
       !$omp target teams distribute map(to: a(1:N, 1:8)) &
       !$omp& reduction(*:results(x)) map(tofrom: results(1:N))
       DO y = 1, 8
          results(x) = a(x, y) * results(x)
       END DO
    END DO

    DO x = 1, N
       IF (host_results(x) .ne. results(x)) THEN
          errors = errors+ 1
       END IF
    END DO

    test_multiply = errors
  END FUNCTION test_multiply
END PROGRAM test_target_teams_distribute_device
