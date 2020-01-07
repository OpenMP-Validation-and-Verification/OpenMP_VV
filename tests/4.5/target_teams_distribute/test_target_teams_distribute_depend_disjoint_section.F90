!===--- test_target_teams_distribute_depend_disjoint_section.F90------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test checks the out-out dependency of two tasks when the array
! sections in the depend lists are disjoint (non-overlapping). If no race
! condition can be shown, then the test gives only a warning, since this is
! still complaint. This test will always pass.
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

  OMPVV_TEST_VERBOSE(depend_disjoint_section() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION depend_disjoint_section()
    INTEGER :: x
    LOGICAL :: all_valid, race_found
    INTEGER,DIMENSION(N):: a, b, c

    all_valid = .TRUE.
    race_found = .FALSE.

    DO x = 1, N
       a(x) = x
       b(x) = 2 * x
       c(x) = 0
    END DO

    !$omp target data map(to: a(1:N), b(1:N)) map(tofrom: c(1:N))
    !$omp target teams distribute nowait depend(in: c(1:(N/2))) map(alloc: &
    !$omp& a(1:N), b(1:N), c(1:N))
    DO x = 1, N
       c(x) = c(x) + a(x) + b(x)
    END DO
    !$omp target teams distribute nowait depend(in: c(((N/2) + 1):N)) map(alloc: &
    !$omp& a(1:N), b(1:N), c(1:N))
    DO x = 1, N
       c(x) = c(x) + 2 * (a(x) + b(x))
    END DO
    !$omp end target data

    DO x = 1, N
       IF ((c(x) .ne. 3 * x) .or. (c(x) .ne. 6 * x) .or. &
            & (c(x) .ne. 9 * x)) THEN
          all_valid = .FALSE.
       END IF
       IF ((c(x) .eq. 3 * x) .or. (c(x) .eq. 6 * x)) THEN
          race_found = .TRUE.
       END IF
    END DO

    IF ((all_valid .eqv. .FALSE.) .or. (race_found .eqv. .FALSE.)) THEN
       OMPVV_WARNING("Test could not prove asyncronous operations of")
       OMPVV_WARNING("tasks dependent on disjoint array sections")
    END IF

    depend_disjoint_section = 0
  END FUNCTION depend_disjoint_section
END PROGRAM test_target_teams_distribute_depend
