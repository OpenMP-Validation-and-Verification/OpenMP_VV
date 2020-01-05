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

  OMPVV_TEST_VERBOSE(depend_list() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION depend_list()
    INTEGER:: errors, x
    INTEGER,DIMENSION(N):: a, b, c, d, e, f, g

    errors = 0

    DO x = 1, N
       a(x) = x
       b(x) = 2 * x
       c(x) = 0
       d(x) = 0
       e(x) = 0
       f(x) = 0
       g(x) = 0
    END DO

    !$omp target data map(to: a(1:N), b(1:N)) map(alloc: c(1:N), d(1:N), &
    !$omp& e(1:N)) map(from: f(1:N), g(1:N))
    !$omp target teams distribute nowait depend(out: c) map(alloc: &
    !$omp& a(1:N), b(1:N), c(1:N))
    DO x = 1, N
       c(x) = a(x) + b(x)
    END DO
    !$omp target teams distribute nowait depend(out: d) map(alloc: &
    !$omp a(1:N), b(1:N), d(1:N))
    DO x = 1, N
       d(x) = a(x) + b(x) + x
    END DO
    !$omp target teams distribute nowait depend(out: c, d, e) map(alloc:&
    !$omp& c(1:N), d(1:N), e(1:N))
    DO x = 1, N
       e(x) = c(x) + d(x)
    END DO
    !$omp target teams distribute nowait depend(out: e) map(alloc: &
    !$omp& a(1:N), e(1:N), f(1:N))
    DO x = 1, N
       f(x) = e(x) + a(x)
    END DO
    !$omp target teams distribute nowait depend(out: e) map(alloc: &
    !$omp& b(1:N), e(1:N), g(1:N))
    DO x = 1, N
       g(x) = e(x) + b(x)
    END DO
    !$omp end target data

    DO x = 1, N
       IF ((f(x) .ne. 8 * x) .or. (g(x) .ne. 9 * x)) THEN
          errors = errors + 1
       END IF
    END DO

    depend_list = errors
  END FUNCTION depend_list
END PROGRAM test_target_teams_distribute_depend
