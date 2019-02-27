!===--- test_target_teams_distribute_nowait.F90-----------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the nowait clause on a target teams distribute directive and
! uses a barrier to resyncronize the target regions.  Since we can't be sure
! that operations will be asyncronous, we can not test to make sure that
! the regions are executed asynchronously.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

      PROGRAM test_target_teams_distribute_nowait
        USE iso_fortran_env
        USE ompvv_lib
        USE omp_lib
        implicit none
        INTEGER :: errors
        OMPVV_TEST_OFFLOADING()
        errors = 0

        OMPVV_TEST_VERBOSE(test_nowait() .ne. 0)

        OMPVV_REPORT_AND_RETURN()
      CONTAINS
        INTEGER FUNCTION test_nowait()
          INTEGER:: errors, x
          LOGICAL:: is_host
          INTEGER,DIMENSION(N):: a, b, c, d, e, f, g

          DO x = 1, N
            a(x) = x
            b(x) = 2 * x
            c(x) = 0
            d(x) = 3 * x
            e(x) = 4 * x
            f(x) = 0
            g(x) = 0
          END DO

          !$omp target data map(to: a(1:N), b(1:N), d(1:N), e(1:N)) map(from: &
          !$omp& c(1:N), f(1:N), g(1:N)) map(tofrom: is_host)
            !$omp parallel
              !$omp target teams distribute nowait map(alloc: a(1:N), b(1:N), &
              !$omp& c(1:N))
              DO x = 1, N
                c(x) = a(x) + b(x)
              END DO
              !$omp end target teams distribute
              !$omp target teams distribute nowait map(alloc: d(1:N), e(1:N), &
              !$omp& f(1:N))
              DO x = 1, N
                f(x) = d(x) + e(x)
              END DO
              !$omp end target teams distribute
              !$omp barrier
              !$omp target teams distribute map(alloc: is_host, c(1:N), f(1:N), &
              !$omp& g(1:N))
              DO x = 1, N
                is_host = omp_is_initial_device()
                g(x) = c(x) + f(x)
              END DO
              !$omp end target teams distribute
            !$omp end parallel
          !$omp end target data

          IF (is_host) THEN
            OMPVV_WARNING("Test ran on host, nowait cannot be tested")
          END IF

          DO x = 1, N
            IF (c(x) .ne. 3 * x) THEN
              errors = errors + 1
            END IF
            IF (f(x) .ne. 7 * x) THEN
              errors = errors + 1
            END IF
            IF (g(x) .ne. 10 * x) THEN
              errors = errors + 1
            END IF
          END DO

          test_nowait = errors
        END FUNCTION test_nowait
      END PROGRAM test_target_teams_distribute_nowait
