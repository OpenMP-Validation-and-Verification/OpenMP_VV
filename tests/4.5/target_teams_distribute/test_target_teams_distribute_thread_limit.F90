!===--- test_target_teams_distribute_num_threads.F90------------------------===//
!
! OpenMP API Version 4.5 Nov 2015
!
! This test uses the num_threads clause on a target teams distribute directive to
! indicate a requested number of threads to execute the teams distribute region.
! The specifications indicate that the number of threads that are given can be any
! number that is equal to or less than the indicated value. We first run a
! target teams distribute region without the clause to see what the default
! number of threads is, and then we use a value that is less than that in the
! test of the num_threads clause. If the region is run with more threads than
! indicated, the test errors. If the region is run with less threads than
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

  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_VERBOSE(test_threads() .ne. 0)

  OMPVV_REPORT_AND_RETURN()
CONTAINS
  INTEGER FUNCTION test_threads()
    INTEGER:: num_threads, default_threads, errors, x
    errors = 0

    !$omp target teams distribute map(from: default_threads)
    DO x = 1, N
       IF (omp_get_team_num() .eq. 0) THEN
          default_threads = omp_get_thread_limit()
       END IF
    END DO

    OMPVV_WARNING_IF(default_threads .eq. 1, "Test operated with one thread. Cannot test thread_limit clause.")
    OMPVV_TEST_AND_SET(errors, default_threads .lt. 1)
for sure,
    IF (default_threads .gt. 0) THEN
       !$omp target teams distribute map(from: num_threads) &
       !$omp& thread_limit(default_threads / 2)
       DO x = 1, N
          IF (omp_get_team_num() .eq. 0) THEN
             num_threads = omp_get_thread_limit()
          END IF
       END DO

       OMPVV_TEST_AND_SET(errors, num_threads .gt. default_threads / 2)
       OMPVV_WARNING_IF(num_threads .lt. default_threads / 2, "Test limited to fewer threads than were indicated.")
    END IF

    test_threads = errors
  END FUNCTION test_threads
END PROGRAM test_target_teams_distribute_device
