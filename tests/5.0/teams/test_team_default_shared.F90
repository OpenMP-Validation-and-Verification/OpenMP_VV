!===--- test_team_default_shared.F90 ---------------------------------------===//
!
! OpenMP API Version 5.0 Nov 2018
!
! This test uses the default(shared) clause on a teams distribute
! directive. 
! The test aims to validate that when the default(shared) clause
! is present, all variables without explicit data sharing attributes
! will
! be shared within the region. To test this, we test that a data element
! that should be shared due to the default(shared) clause is available
! to
! all the teams. The first test uses atomic to write to the variable
! without
! race conditions. The second test uses synchronization constructs to
! have
! one thread change the shared variable and ensures all threads see the
! change.
!
!//===----------------------------------------------------------------------===//

#include "ompvv.F90"

#define N 1024

PROGRAM test_teams_distribute_default_shared

  USE iso_fortran_env
  USE ompvv_lib
  USE omp_lib
  implicit none

  OMPVV_TEST_VERBOSE(test_default_shared() .ne. 0)

  OMPVV_REPORT_AND_RETURN()

CONTAINS
  INTEGER FUNCTION test_default_shared()
  CHARACTER(len=300):: infoMsg
  INTEGER,DIMENSION(N):: a
  INTEGER:: share, errors, num_teams
  errors = 0
  share = 0 !can I do this on above line? multiple variable declarations
in F90?

  DO x = 1, N
    a[x] = x
  END DO

  !$omp target teams distribute default(shared) 
  !$omp& num_teams(OMPVV_NUM_TEAMS_DEVICE)
  DO x = 1, omp_get_num_teams
    IF (omp_get_num_teams() .eq. 0) THEN
      num_teams = omp_get_num_teams();
    END IF

  !$omp atomic 
  share = share + 1

  OMPVV_WARNING_IF(num_teams .eq. 1, "Test operated on one team, results
of default shared test are inconclusive.");

  test_default_shared = errors
END PROGRAM test_target_teams_distribute_default_shared
