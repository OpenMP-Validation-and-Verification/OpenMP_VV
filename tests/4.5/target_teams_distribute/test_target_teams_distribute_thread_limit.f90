!===--- test_target_teams_distribute_num_threads.F90------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the num_threads clause on a target teams distribute directive to
! indicate a requested number of threads to execute the teams distribute region.
! The specifications indicate that the number of threads that are given can be any
! number that is equal to or less than the indicated value.  We first run a
! target teams distribute region without the clause to see what the default
! number of threads is, and then we use a value that is less than that in the
! test of the num_threads clause.  If the region is run with more threads than
! indicated, the test errors.  If the region is run with less threadsthan
! indicated, the test issues a warning since it is known that the device can
! run with more threads than was actually given.
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

        OMPVV_TEST_VERBOSE(test_threads() .ne. 0)

        OMPVV_REPORT_AND_RETURN()
      CONTAINS
        INTEGER FUNCTION test_threads()
          INTEGER:: num_threads, default_threads, errors

          !$omp target teams distribute map(from: default_threads)
          DO x = 1, N
            default_threads = omp_get_thread_limit()
          END DO

          IF (default_threads .eq. 1) THEN
            OMPVV_WARNING("Test operated with one thread. Testing of thread")
            OMPVV_WARNING("clause cannot be done.")
          ELSEIF (default_threads .lt. 1) THEN
            OMPVV_ERROR("Test returned thread limit < 0 which can't be correct")
            errors = errors + 1
          ELSE
            !$omp target teams distribute map(from: num_threads) thread_limit(&
            !$omp& default_threads - 1)
            DO x = 1, N
              num_threads = omp_get_thread_limit()
            END DO

            IF (num_threads .gt. default_threads - 1) THEN
              errors = errors + 1
              OMPVV_ERROR("Test ran on more threads than requested.")
            ELSEIF (num_threads .lt. default_threads - 1) THEN
              OMPVV_WARNING("Test ran on less threads than requested")
              OMPVV_WARNING("This behavior is still spec-conformant")
            END IF
          END IF
          test_threads = errors
        END FUNCTION test_threads
      END PROGRAM
