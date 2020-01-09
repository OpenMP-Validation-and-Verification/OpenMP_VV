!===--- test_target_teams_distribute_depend_in_in.F90-----------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test checks if two depend(in) tasks are independent by trying to
! create a race condition. If no race condition can be shown, then the test
! gives only a warning, since this is still complaint. This test will 
! always pass.
!
!//===----------------------------------------------------------------------===//
#include "ompvv.F90"

#define N 1024

PROGRAM test_target_teams_distribute_depend
  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none
  INTEGER :: errors
  OMPVV_TEST_OFFLOADING
  errors = 0

  OMPVV_TEST_VERBOSE(depend_in_in() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION depend_in_in()
    INTEGER :: x, errors
    LOGICAL :: invalid_found, race_found
    INTEGER,DIMENSION(N):: a, b, c, d

    invalid_found = .FALSE.
    race_found = .FALSE.
    errors = 0

    DO x = 1, N
       a(x) = x
       b(x) = 2*x
       c(x) = 0
       d(x) = 0
    END DO

    !$omp target data map(to: a(1:N), b(1:N)) map(tofrom: c(1:N), d(1:N))
    !$omp target teams distribute nowait depend(in:d) map(alloc: &
    !$omp& a(1:N), b(1:N), d(1:N))
    DO x = 1, N
       d(x) = d(x) + a(x) + b(x)
    END DO
    !$omp target teams distribute nowait depend(in:d) map(alloc: &
    !$omp& a(1:N), b(1:N), c(1:N), d(1:N))
    DO x = 1, N
       c(x) = c(x) + 2*(a(x) + b(x)) + d(x)
    END DO
    !$omp end target data

    DO x = 1, N
       IF ((c(x) .ne. 6*x) .and. (c(x) .ne. 9*x)) THEN
          invalid_found = .TRUE.
          errors = 1
          OMPVV_ERROR("Found invalid values")
          exit
       END IF
       IF (c(x) .eq. 6*x) THEN
          race_found = .TRUE.
       END IF
    END DO

    IF ((invalid_found .eqv. .FALSE.) .and. (race_found .eqv. .TRUE.)) THEN
       OMPVV_INFOMSG("Found independence between depend(in) clauses")
    ELSE
       OMPVV_WARNING("Could not find independence between depend(in) clauses")
    END IF

    depend_in_in = errors
  END FUNCTION depend_in_in
END PROGRAM test_target_teams_distribute_depend
