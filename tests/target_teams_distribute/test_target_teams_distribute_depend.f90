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
        OMPVV_TEST_OFFLOADING()
        errors = 0

        OMPVV_TEST_VERBOSE(depend_in_in() .ne. 0)
        OMPVV_TEST_VERBOSE(depend_in_out() .ne. 0)
        OMPVV_TEST_VERBOSE(depend_out_in() .ne. 0)
        OMPVV_TEST_VERBOSE(depend_out_out() .ne. 0)
        OMPVV_TEST_VERBOSE(depend_array_secion() .ne. 0)
        OMPVV_TEST_VERBOSE(depend_disjoint_section() .ne. 0)
        OMPVV_TEST_VERBOSE(depend_list() .ne. 0)
        OMPVV_TEST_VERBOSE(depend_unused_data() .ne. 0)

        OMPVV_REPORT_AND_RETURN()

      CONTAINS
        INTEGER FUNCTION depend_in_in()
          INTEGER :: x
          INTEGER, DIMENSION(N):: a, b, c
          LOGICAL:: race_found, all_valid

          all_valid = .TRUE.
          race_found = .FALSE.

          DO x = 1, N
            a(x) = x
            b(x) = 2 * x
            c(x) = 0
          END DO

          !$omp target data map(to: a(1:N), b(1:N)) map(tofrom:c(1:N))
            !$omp target teams distribute nowait depend(in: c) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = c(x) + a(x) + b(x)
            END DO
            !$omp target teams distribute nowait depend(in:c) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = c(x) + 2 * a(x)
            END DO
          !$omp end target data

          DO x = 1, N
            IF ((c(x) .eq. 3 * x) .or. (c(x) .eq. 6 * x) .or. (c(x) .eq. 9 * x &
              & )) THEN
              all_valid = .FALSE.
            END IF
            IF ((c(x) .eq. 3 * x) .or. (c(x) .eq. 6 * x)) THEN
              race_found = .TRUE.
            END IF
          END DO

          IF ((all_valid .eqv. .FALSE) .or. (race_found .eqv. .FALSE.)) THEN
            OMPVV_WARNING("Could not prove asyncronous operations of ")
            OMPVV_WARNING("depend(in) task with other depend(in) task")
          END IF
          depend_in_in = 0
        END FUNCTION depend_in_in

        INTEGER FUNCTION depend_in_out()
          INTEGER :: errors_a, errors_b, x
          INTEGER, DIMENSION(N) :: a, b, c, d

          errors_a = 0
          errors_b = 0

          DO x = 1, N
            a(x) = x
            b(x) = 2 * x
            c(x) = 0
            d(x) = 0
          END DO

          !$omp target data map(to: a(1:N), b(1:N)) map(alloc: c(1:N)) map( &
          !$omp& d(1:N))
            !$omp target teams distribute nowait depend(in: c) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = a(x) + b(x)
            END DO
            !$omp target teams distribute nowait depend(out: c) map(alloc: &
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
          !$omp from: d(1:N))
            !$omp target teams distribute nowait depend(in: c) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = a(x) + b(x)
            END DO
            !$omp target teams distribute nowait depend(inout: c) map(alloc: &
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
            OMPVV_ERROR("Both out/inout dependencies were not dependent")
            OMPVV_ERROR("on depend(in) task")
          ELSEIF (errors_b .gt. 0) THEN
            OMPVV_ERROR("Only inout dependencies were not dependent on")
            OMPVV_ERROR("depend(in) task")
          ELSEIF (errors_a .gt. 0) THEN
            OMPVV_ERROR("Only out dependencies were not dependent on ")
            OMPVV_ERROR("depend(in) task")
          END IF

          depend_in_out = errors_a + errors_b
        END FUNCTION depend_in_out

        INTEGER FUNCTION depend_out_in()
          INTEGER:: errors_a, errors_b, x
          INTEGER,DIMENSION(N):: a, b, c, d

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
            !$omp target teams distribute nowait depend(inout:c) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = a(x) + b(x)
            END DO
            !$omp target teams distribute nowait depend(in:c) map(alloc: &
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

        INTEGER FUNCTION depend_out_out()
          INTEGER:: out_out_errors, inout_out_errors, out_inout_errors
          INTEGER:: inout_inout_errors, x
          INTEGER, DIMENSION(N):: a, b, c, d

          out_out_errors = 0
          inout_out_errors = 0
          out_inout_errors = 0
          inout_inout_errors = 0

          DO x = 1, N
            a(x) = x
            b(x) = 2 * x
            c(x) = 0
            d(x) = 0
          END DO

          !$omp target data map(to: a(1:N), b(1:N)) map(alloc: c(1:N)) map(&
          !$omp& from: d(1:N))
            !$omp target teams distribute nowait depend(out: c) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = a(x) + b(x)
            END DO
            !$omp target teams distribute nowait depend(out: c) map(alloc: &
            !$omp& b(1:N), c(1:N), d(1:N))
            DO x = 1, N
              d(x) = c(x) + b(x)
            END DO
          !$omp end target data

          DO x = 1, N
            IF (d(x) .ne. 5 * x) THEN
              out_out_errors = out_out_errors + 1
            END IF
          END DO

          !$omp target data map(to: a(1:N), b(1:N)) map(alloc: c(1:N)) map(&
          !$omp& from: d(1:N))
            !$omp target teams distribute nowait depend(out:c) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = a(x) + b(x)
            END DO
            !$omp target teams distribute nowait depend(inout:c) map(alloc: &
            !$omp& a(1:N), c(1:N), d(1:N))
            DO x = 1, N
              d(x) = a(x) + c(x)
            END DO
          !$omp end target data

          DO x = 1, N
            IF (d(x) .ne. 4 * x) THEN
              out_inout_errors = out_inout_errors + 1
            END IF
          END DO

          !$omp target data map(to: a(1:N), b(1:N)) map(alloc: c(1:N)) map(&
          !$omp& from: d(1:N))
            !$omp target teams distribute nowait depend(inout:c) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = a(x) + b(x)
            END DO
            !$omp target teams distribute nowait depend(out:c) map(alloc: &
            !$omp& b(1:N), c(1:N), d(1:N))
            DO x = 1, N
              d(x) = b(x) + c(x)
            END DO
          !$omp end target data

          DO x = 1, N
            IF (d(x) .ne. 5 * x) THEN
              inout_out_errors = inout_out_errors + 1
            END IF
          END DO

          !$omp target data map(to: a(1:N), b(1:N)) map(alloc: c(1:N)) map(&
          !$omp& from: d(1:N))
            !$omp target teams distribute nowait depend(inout:c) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = a(x) + b(x)
            END DO
            !$omp target teams distribute nowait depend(inout:c) map(alloc: &
            !$omp& a(1:N), c(1:N), d(1:N))
            DO x = 1, N
              d(x) = a(x) + c(x)
            END DO
          !$omp end target data

          DO x = 1, N
            IF (d(x) .ne. 4 * x) THEN
              inout_inout_errors = inout_inout_errors + 1
            END IF
          END DO

          IF ((inout_inout_errors + inout_out_errors + out_inout_errors + &
          & out_out_errors) .gt. 0) THEN
            OMPVV_ERROR("The following task dependencies fail: ")
          END IF
          IF (inout_inout_errors .gt. 0) THEN
            OMPVV_ERROR("inout task on inout task")
          END IF
          IF (inout_out_errors .gt. 0) THEN
            OMPVV_ERROR("inout task on out task")
          END IF
          IF (out_inout_errors .gt. 0) THEN
            OMPVV_ERROR("out task on inout task")
          END IF
          IF (out_out_errors .gt. 0) THEN
            OMPVV_ERROR("out task on out task")
          END IF

          depend_out_out = inout_inout_errors + inout_out_errors + &
          & out_inout_errors + out_out_errors
        END FUNCTION depend_out_out

        INTEGER FUNCTION depend_array_section()
          INTEGER :: errors, x
          INTEGER,DIMENSION(N) :: a, b, c, d

          DO x = 1, N
            a(x) = x
            b(x) = 2 * x
            c(x) = 0
            d(x) = 0
          END DO

          !$omp target data map(to: a(1:N), b(1:N)) map(alloc: c(1:N)) map( &
          !$omp& from: d(1:N))
            !$omp target teams distribute nowait depend(out: c(1:N)) map(alloc &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = a(x) + b(x)
            END DO
            !$omp target teams distribute nowait depend(out: c(1:N)) map(alloc &
            !$omp& b(1:N), c(1:N), d(1:N))
            DO x = 1, N
              d(x) = a(x) + b(x)
            END DO
          !$omp end target data

          DO x = 1, N
            IF (d(x) .ne. 5 * x) THEN
              errors = errors + 1
            END IF
          END DO

          depend_array_section = errors
        END FUNCTION depend_array_section

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
            !$omp target teams distribute nowait depend(c(1:2)) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              c(x) = c(x) + a(x) + b(x)
            END DO
            !$omp target teams distribute nowait depend(c(3:N)) map(alloc: &
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

          IF ((all_valid .eqv. .FALSE.) .or. (race_found .eqv. .FALSE)) THEN
            OMPVV_WARNING("Test could not prove asyncronous operations of")
            OMPVV_WARNING("tasks dependent on disjoint array sections")
          END IF

          depend_disjoint_section = 0
        END FUNCTION depend_disjoint_section

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

          !$omp target data map(to: a(1:N), b(1:N)) map(aloc: c(1:N), d(1:N), &
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
            !$omp target teams distribute nowait depend(out: c,d,e) map(alloc:&
            !$omp& c(1:n), d(1:N), e(1:N))
            DO x = 1, N
              e(x) = c(x) + d(x)
            END DO
            !$omp target teams distribute nowait depend(out: e) map(alloc: &
            !$omp& a(1:N), e(1:N), f(1:N))
            DO x = 1, N
              f(x) = e(x) + a(x)
            END DO
            !$omp target teams distribute nowait depend(out: e) map(alloc: &
            !$omp& b(1:N, e(1:N), g(1:N))
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
