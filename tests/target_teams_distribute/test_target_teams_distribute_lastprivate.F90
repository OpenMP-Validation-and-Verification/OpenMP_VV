!===--- test_target_teams_distribute_lastprivate.F90------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the lastprivate clause to indicate that the privatized value
! that is passed as the parameter should also be returned with the value that
! results from the thread that runs the last iteration of the for loop in the
! target teams distribute directive.  The clause can be used with both scalar
! and array data types and both situations are tested.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

      PROGRAM test_target_teams_distribute_lastprivate
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        INTEGER :: errors
        OMPVV_TEST_OFFLOADING()
        errors = 0

        OMPVV_TEST_VERBOSE(test_scalar() .ne. 0)
        OMPVV_TEST_VERBOSE(test_array() .ne. 0)

        OMPVV_REPORT_AND_RETURN()
      CONTAINS
        INTEGER FUNCTION test_scalar()
          INTEGER:: x, errors, privatized
          INTEGER,DIMENSION(N):: a, b, c

          DO x = 1, N
            a(x) = 1
            b(x) = x
            c(x) = 0
          END DO

          errors = 0

          !$omp target data map(to: a(1:N), b(1:N)) map(tofrom: c(1:N))
            !$omp target teams distribute lastprivate(privatized) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              privatized = a(x) - b(x)
              c(x) = privatized + b(x)
            END DO
          !$omp end target data

          DO x = 1, N
            IF (c(x) .ne. a(x)) THEN
              errors = errors + 1
            END IF
          END DO

          IF (privatized .ne. (a(N) - b(N))) THEN
            errors = errors + 1
          END IF

          test_scalar = errors
        END FUNCTION test_scalar

        INTEGER FUNCTION test_array()
          INTEGER:: x, errors
          INTEGER,DIMENSION(N):: a, b, c
          INTEGER,DIMENSION(2):: privatized

          DO x = 1, N
            a(x) = 1
            b(x) = x
            c(x) = MOD(x, 10)
          END DO

          !$omp target data map(to: a(1:N), b(1:N), c(1:N)) map(tofrom: &
          !$omp& privatized(1:2))
            !$omp target teams distribute lastprivate(privatized) map(alloc: &
            !$omp& a(1:N), b(1:N), c(1:N))
            DO x = 1, N
              privatized(1) = a(x) + b(x) + c(x)
              privatized(2) = (a(x) + b(x)) * c(x)
            END DO
          !$omp end target data

          IF (privatized(1) .ne. a(N) + b(N) + c(N)) THEN
            errors = errors + 1
          END IF
          IF (privatized(2) .ne. (a(N) + b(N)) * c(N)) THEN
            errors = errors + 1
          END IF

          test_array = errors
        END FUNCTION test_array
      END PROGRAM test_target_teams_distribute_lastprivate
