!===------ test_target_teams_distribute_dist_schedule.F90 ----------------===//
! 
! OpenMP API Version 4.5 Nov 2015
! 
! This test checks that the dist_schedule clause (which must have kind 
! static) correctly causes CHUNK_SIZE iterations to be split among the
! number of teams the test is run with, in a round-robin faction in order
! of the team number, when a chunk size is given. The test also confirms
! that when no chunk size is given, that each team receives no more than
! one chunk.
!
!===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024
#define CHUNK_SIZE 64

PROGRAM main
  USE iso_fortran_env
  USE ompvv_lib
  implicit none

  OMPVV_TEST_OFFLOADING

  OMPVV_TEST_SHARED_ENVIRONMENT

  OMPVV_TEST_VERBOSE(test_function() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_dist_schedule() 
    INTEGER:: errors
    test_function = 1;
    !$omp target map(from: errors)
    test_function = 0;
    !$omp end target
    test_dist_schedule = errors
  END FUNCTION test_dist_schedule
END PROGRAM main
