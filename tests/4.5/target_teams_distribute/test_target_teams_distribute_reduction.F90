!===--- test_target_teams_distribute_reduction.F90--------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the reduction clause on a target teams distribute directive,
! testing, for each operator, that the variable in the reduction clause is
! properly reduced.
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

        OMPVV_TEST_OFFLOADING()

        OMPVV_TEST_VERBOSE(test_add() .ne. 0)
        OMPVV_TEST_VERBOSE(test_and() .ne. 0)
        OMPVV_TEST_VERBOSE(test_bitand() .ne. 0)
        OMPVV_TEST_VERBOSE(test_bitor() .ne. 0)
        OMPVV_TEST_VERBOSE(test_bitxor() .ne. 0)
        OMPVV_TEST_VERBOSE(test_max() .ne. 0)
        OMPVV_TEST_VERBOSE(test_min() .ne. 0)
        OMPVV_TEST_VERBOSE(test_multiply() .ne. 0)
        OMPVV_TEST_VERBOSE(test_or() .ne. 0)
        OMPVV_TEST_VERBSOE(test_eqv() .ne. 0)
        OMPVV_TEST_VERBOSE(test_neqv() .ne. 0)
        OMPVV_TEST_VERBOSE(test_sub() .ne. 0)

        OMPVV_REPORT_AND_RETURN()
      CONTAINS
        INTEGER FUNCTION test_add()
          INTEGER,DIMENSION(N):: a, b
          INTEGER:: x, dev_sum, host_sum, errors
          errors = 0
          host_sum = 0
          dev_sum = 0

          DO x = 1, N
            a(x) = 1
            b(x) = x
          END DO

          DO x = 1, N
            host_sum = host_sum + a(x) + b(x)
          END DO

          !$omp target teams distribute map(to: a(1:N), b(1:N)) &
          !$omp& reduction(+:dev_sum)
          DO x = 1, N
            dev_sum = a(x) + b(x) + dev_sum
          END DO

          IF (dev_sum .ne. host_sum) THEN
            errors = errors + 1
          END IF
          test_add = errors
        END FUNCTION test_add

        INTEGER FUNCTION test_and()
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
            DO x = 1, N
              IF (randoms(x) .lt. false_margin) THEN
                a(x) = .TRUE.
              ELSE
                a(x) = .FALSE.
              END IF
            END DO

            result = .TRUE.
            host_result = .TRUE.

            DO x = 1, N
              host_result = host_result .AND. a(x)
            END DO

            !$omp target teams distribute map(to: a(0:N)) &
            !$omp& reduction(.and.: result)
            DO x = 1, N
              result = a(x) .AND. result
            END DO

            IF (result .neqv. host_result) THEN
              errors = errors + 1
            END IF
          END DO

          test_and = errors
        END FUNCTION test_and

        INTEGER FUNCTION test_bitand()
          INTEGER,DIMENSION(N):: a
          REAL(8),DIMENSION(N, 32):: randoms
          INTEGER:: result, host_result, x, y, z, errors
          REAL(8):: false_margin
          result = 0

          CALL RANDOM_SEED()
          false_margin = exp(log(.5) / N)

          DO y = 1, 32
            CALL RANDOM_NUMBER(randoms)
            DO x = 1, N
              a(x) = 0
              DO z = 1, 32
                IF (randoms(x, y) .lt. false_margin) THEN
                  a(x) = a(x) + (2**z)
                END IF
              END DO
            END DO

            result = 0
            host_result = 0
            DO z = 1, 32
              result = result + (2**z)
              host_result = host_result + (2**z)
            END DO

            DO x = 1, N
              host_result = iand(a(x), host_result)
            END DO

            !$omp target teams distribute map(to:a(1:N)) reduction(iand:result)
            DO x = 1, N
              result = iand(a(x), result)
            END DO

            IF (host_result .ne. result) THEN
              errors = errors + 1
            END IF
          END DO

          test_bitand = errors
        END FUNCTION test_bitand

        INTEGER FUNCTION test_bitor()
          INTEGER,DIMENSION(N) :: a
          REAL(8),DIMENSION(N, 32):: randoms
          INTEGER:: result, host_result, x, y, z, errors
          REAL(8):: false_margin
          errors = 0

          CALL RANDOM_SEED()
          false_margin = exp(log(.5) / N)

          DO y = 1, 32
            CALL RANDOM_NUMBER(randoms)
            host_result = 0
            result = 0
            DO x = 1, N
              a(x) = 0
              DO z = 1, 32
                IF (randoms(x, y) .gt. false_margin) THEN
                  a(x) = a(x) + (2**z)
                END IF
              END DO
            END DO

            DO x = 1, N
              host_result = ior(a(x), host_result)
            END DO

            !$omp target teams distribute map(to:a(1:N)) reduction(ior:result)
            DO x = 1, N
              result = ior(a(x), result)
            END DO

            IF (host_result .ne. result) THEN
              errors = errors + 1
            END IF
          END DO

          test_bitor = errors
        END FUNCTION test_bitor

        INTEGER FUNCTION test_bitxor()
          INTEGER,DIMENSION(N):: a
          INTEGER:: result, host_result, x, y, errors
          errors = 0

          CALL RANDOM_SEED()

          DO y = 1, 32
            CALL RANDOM_NUMBER(a)
            host_result = 0
            result = 0

            DO x = 1, N
              host_result = ieor(a(x), host_result)
            END DO

            !$omp target teams distribute map(to: a(1:N)) reduction(ixor:result)
            DO x = 1, N
              result = ieor(a(x), result)
            END DO

            IF (result .ne. host_result) THEN
              errors = errors + 1
            END IF
          END DO

          test_bitxor = errors
        END FUNCTION test_bitxor

        INTEGER FUNCTION test_max()
          INTEGER,DIMENSION(N):: a
          INTEGER:: result, x, y, errors
          errors = 0

          DO y = 1, 32
            DO x = 1, N
              a(x) = x + y
            END DO
            result = a(1)

            !$omp target teams distribute map(to:a(1:N)) reduction(max:result)
            DO x = 1, N
              result = max(a(x), result)
            END DO

            IF (result .ne. y + N) THEN
              errors = errors + 1
            END IF
          END DO

          test_max = errors
        END FUNCTION test_max

        INTEGER FUNCTION test_min()
          INTEGER,DIMENSION(N):: a
          INTEGER:: result, x, y, errors
          errors = 0

          DO y = 1, 32
            DO x = 1, N
              a(x) = x + y
            END DO

            result = a(N)

            !$omp target teams distribute map(to:a(1:N)) reduction(min:result)
            DO x = 1, N
              result = min(a(x), result)
            END DO

            IF (result .ne. 1 + y) THEN
              errors = errors + 1
            END IF
          END DO

          test_min = errors
        END FUNCTION test_min

        INTEGER FUNCTION test_multiply()
          INTEGER,DIMENSION(N, 8):: a
          INTEGER,DIMENSION(N):: results, host_results
          INTEGER:: x, y, errors

          errors = 0
          host_results = 1
          results = 1

          DO x = 1, N
            DO y = 1, 8
              a(x, y) = MOD(x, 8) + y
            END DO
          END DO

          DO x = 1, N
            DO y = 1, 8
              host_results(x) = a(x, y) * host_results(x)
            END DO
          END DO

          DO x = 1, N
            !$omp target teams distribute map(to:a(1:N, 1:N)) &
            !$omp& reduction(*:results(x))
            DO y = 1, 8
              results(x) = a(x, y) * results(x)
            END DO
          END DO

          DO x = 1, N
            IF (host_results(x) .ne. results(x)) THEN
              errors = errors+ 1
            END IF
          END DO

          test_multiply = errors
        END FUNCTION test_multiply

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
            !$omp& result)
            DO x = 1, N
              result = a(x) .OR. result
            END DO

            IF (host_result .neqv. result) THEN
              errors = errors + 1
            END IF
          END DO

          test_or = errors
        END FUNCTION test_or

        INTEGER FUNCTION test_eqv()
          LOGICAL,DIMENSION(N):: a
          REAL(8),DIMENSION(N):: randoms
          INTEGER:: x, y, errors
          LOGICAL:: host_result, result
          errors = 0
          CALL RANDOM_SEED()

          DO y = 1, 32
            CALL RANDOM_NUMBER(randoms)
            a = .TRUE.
            host_result = .TRUE.
            result = .TRUE.
            DO x = 1, N
              IF (randoms(x) .gt. .5) THEN
                a(x) = .FALSE.
              END IF
            END DO

            DO x = 1, N
              host_result = a(x) .eqv. host_result
            END DO

            !$omp target teams distribute map(to:a(1:N)) reduction(eqv:result)
            DO x = 1, N
              result = a(x) .eqv. result
            END DO

            IF (host_result .neqv. result) THEN
              errors = errors + 1
            END IF
          END DO

          test_eqv = errors
        END FUNCTION test_eqv

        INTEGER FUNCTION test_neqv()
          LOGICAL,DIMENSION(N):: a
          REAL(8),DIMENSION(N):: randoms
          INTEGER:: x, y, errors
          LOGICAL:: host_result, result
          errors = 0
          CALL RANDOM_SEED()

          DO y = 1, 32
            CALL RANDOM_NUMBER(randoms)
            a = .TRUE.
            host_result = .TRUE.
            result = .TRUE.
            DO x = 1, N
              IF (randoms(x) .gt. .5) THEN
                a(x) = .FALSE.
              END IF
            END DO

            DO x = 1, N
              host_result = a(x) .neqv. host_result
            END DO

            !$omp target teams distribute map(to:a(1:N)) reduction(neqv:result)
            DO x = 1, N
              result = a(x) .neqv. result
            END DO

            IF (host_result .neqv. result) THEN
              errors = errors + 1
            END IF
          END DO

          test_neqv = errors
        END FUNCTION test_neqv

        INTEGER FUNCTION test_sub()
          INTEGER,DIMENSION(N):: a
          INTEGER:: x, y, errors, host_result, result

          DO y = 1, N
            result = 0
            host_result = 0

            DO x = 1, N
              a(x) = x + y
            END DO

            DO x = 1, N
              host_result = host_result - a(x)
            END DO

            !$omp target teams distribute map(to:a(1:N)) reduction(result)
            DO x = 1, N
              result = result - a(x)
            END DO

            IF (result .ne. host_result) THEN
              errors = errors + 1
            END IF
          END DO

          test_sub = errors
        END FUNCTION test_sub
      END PROGRAM
