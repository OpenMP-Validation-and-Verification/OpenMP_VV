!===--- test_target_teams_distribute_reduction_or.F90-----------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the reduction clause on a target teams distribute
! directive, testing, for the or operator, that the variable in the
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

  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(test_or() .ne. 0)

  OMPVV_REPORT_AND_RETURN()
CONTAINS
  INTEGER FUNCTION test_or()
    LOGICAL,DIMENSION(N):: a
    REAL(8),DIMENSION(N):: randoms
    REAL(8):: false_margin
    LOGICAL:: result, host_result
    INTEGER:: x, y, errors
    errors = 0

    false_margin = exp(log(.5) / N)
    CALL RANDOM_SEED()

    DO y = 1, 32
       CALL RANDOM_NUMBER(randoms)
       host_result = .FALSE.
       result = .FALSE.
       DO x = 1, N
          IF (randoms(x) .gt. false_margin) THEN
             a(x) = .TRUE.
          ELSE
             a(x) = .FALSE.
          END IF
       END DO

       DO x = 1, N
          host_result = a(x) .OR. host_result
       END DO

       !$omp target teams distribute map(to: a(1:N)) reduction(.or.: &
       !$omp& result) map(tofrom: result)
       DO x = 1, N
          result = a(x) .OR. result
       END DO

       IF (host_result .neqv. result) THEN
          errors = errors + 1
       END IF
    END DO

    test_or = errors
  END FUNCTION test_or
END PROGRAM test_target_teams_distribute_device
