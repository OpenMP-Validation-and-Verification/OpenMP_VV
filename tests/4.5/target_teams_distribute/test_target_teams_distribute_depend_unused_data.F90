!===--- test_target_teams_distribute_depend.F90-----------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test defines a series of functions that enumerate the possible
! combinations of the interactions of the depends clause with the various
! dependence-types: in, out, inout.  With each combination, it tests if
! the dependence between them (if necessary) is forced.  If there is no
! required dependence, then the test tries to see if race conditions between
! the two independent target regions can be formed.  However, if it fails
! to do so, it only issues a warning as this is both a imperfect test of
! the independence and it is not requried that they both execute at the
! same time.
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

  OMPVV_TEST_VERBOSE(depend_unused_data() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION depend_unused_data()
    INTEGER:: errors, x
    INTEGER,DIMENSION(N):: a, b, c, d
    INTEGER,DIMENSION(1):: random_data

    errors = 0

    DO x = 1, N
       a(x) = x
       b(x) = 2 * x
       c(x) = 0
       d(x) = 0
    END DO

    !$omp target data map(to: a(1:N), b(1:N)) map(alloc: c(1:N), &
    !$omp& random_data(1:1)) map(from: d(1:N))
    !$omp target teams distribute nowait depend(out: random_data) &
    !$omp map(alloc: a(1:N), b(1:N), c(1:N))
    DO x = 1, N
       c(x) = a(x) + b(x)
    END DO
    !$omp target teams distribute nowait depend(out: random_data) &
    !$omp& map(alloc: d(1:N), c(1:N), b(1:N))
    DO x = 1, N
       d(x) = c(x) + b(x)
    END DO
    !$omp end target data

    DO x = 1, N
       IF (d(x) .ne. 5 * x) THEN
          errors = errors + 1
       END IF
    END DO

    depend_unused_data = errors
  END FUNCTION depend_unused_data
END PROGRAM test_target_teams_distribute_depend
