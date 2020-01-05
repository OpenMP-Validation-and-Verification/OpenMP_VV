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

  OMPVV_TEST_VERBOSE(depend_out_in() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION depend_out_in()
    INTEGER:: errors_a, errors_b, x
    INTEGER,DIMENSION(N):: a, b, c, d

    DO x = 1, N
       a(x) = x
       b(x) = 2 * x
       c(x) = 0
       d(x) = 0
    END DO

    errors_a = 0
    errors_b = 0

    !$omp target data map(to: a(1:N), b(1:N)) map(alloc: c(1:N)) map(&
    !$omp& from: d(1:N))
    !$omp target teams distribute nowait depend(out: c) map(alloc: &
    !$omp& b(1:N), c(1:N), d(1:N))
    DO x = 1, N
       c(x) = a(x) + b(x)
    END DO
    !$omp target teams distribute nowait depend(in: c) map(alloc: &
    !$omp& b(1:N), c(1:N), d(1:N))
    DO x = 1, N
       d(x) = c(x) + b(x)
    END DO
    !$omp end target data

    DO x = 1, N
       IF (d(x) .ne. 5 * x) THEN
          errors_a = errors_a + 1
       END IF
    END DO

    !$omp target data map(to: a(1:N), b(1:N)) map(alloc: c(1:N)) map( &
    !$omp& from: d(1:N))
    !$omp target teams distribute nowait depend(inout: c) map(alloc: &
    !$omp& a(1:N), b(1:N), c(1:N))
    DO x = 1, N
       c(x) = a(x) + b(x)
    END DO
    !$omp target teams distribute nowait depend(in: c) map(alloc: &
    !$omp& a(1:N), c(1:N), d(1:N))
    DO x = 1, N
       d(x) = c(x) + a(x)
    END DO
    !$omp end target data

    DO x = 1, N
       IF (d(x) .ne. 4 * x) THEN
          errors_b = errors_b + 1
       END IF
    END DO

    IF ((errors_a .gt. 0) .and. (errors_b .gt. 0)) THEN
       OMPVV_ERROR("In dependencies were not dependent on either")
       OMPVV_ERROR("on depend(inout) or depend(out) task")
    ELSEIF (errors_b .gt. 0) THEN
       OMPVV_ERROR("In dependencies were not dependent on only")
       OMPVV_ERROR("depend(inout) task")
    ELSEIF (errors_a .gt. 0) THEN
       OMPVV_ERROR("In dependencies were not dependent on only")
       OMPVV_ERROR("depend(out) task")
    END IF

    depend_out_in = errors_a + errors_b
  END FUNCTION depend_out_in
END PROGRAM test_target_teams_distribute_depend
