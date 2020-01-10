!===--- test_target_teams_distribute_reduction_bitor.F90--------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This test uses the reduction clause on a target teams distribute
! directive, testing, for the bitor operator, that the variable in the
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

  OMPVV_TEST_VERBOSE(test_bitor() .ne. 0)

  OMPVV_REPORT_AND_RETURN()
CONTAINS
  INTEGER FUNCTION test_bitor()
    INTEGER,DIMENSION(N) :: a
    REAL(8),DIMENSION(N, 32):: randoms
    INTEGER:: result, host_result, x, y, z, errors
    REAL(8):: true_margin
    errors = 0

    CALL RANDOM_SEED()
    true_margin = exp(log(.5) / N)

    DO y = 1, 32
       CALL RANDOM_NUMBER(randoms)
       host_result = 0
       result = 0
       DO x = 1, N
          a(x) = 0
          DO z = 1, 32
             IF (randoms(x, y) .gt. true_margin) THEN
                a(x) = a(x) + (2**z)
             END IF
          END DO
       END DO

       DO x = 1, N
          host_result = ior(a(x), host_result)
       END DO

       !$omp target teams distribute map(to: a(1:N)) &
       !$omp& reduction(ior:result) map(tofrom: result)
       DO x = 1, N
          result = ior(a(x), result)
       END DO

       OMPVV_TEST_AND_SET_VERBOSE(errors, host_result .ne. result)
    END DO

    test_bitor = errors
  END FUNCTION test_bitor
END PROGRAM test_target_teams_distribute_device
