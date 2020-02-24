//===--- test_target_teams_distribute_shared.c-------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the shared clause on a target teams distribute directive and
// tests in a few ways that the variable is shared between the teams.  In the
// first test, the atomic directive is used to indicate that all operations on
// the variable should be done atomically.  If the value is the correct value
// at the end of the region, then all teams operated on the same variable.
//
// The second test sets the value of the shared value to a range of values.
// Since the variable is being updated by each team, it is impossible to know
// which value will be the last to be assigned to the variable.  However, we
// test to make sure that the variable is assigned by one of the values that
// could result from the operation.
//
// The third test, instead of writing to the variable, only reads from the
// variable.  This tests that the value of the shared variable has not been
// initiallized improperly or privatized.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define SIZE 1024

int main() {
  int is_offloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(is_offloading);
  int a[SIZE];
  int share = 0;
  int errors = 0;
  int prev_errors = errors;
  int num_teams;

  for (int x = 0; x < SIZE; ++x) {
    a[x] = x;
  }

  // The defaultmap(tofrom:scalar) is used here because the OpenMP 4.5 specification
  // forbids the use of map and data-sharing clauses on the same list item in the 
  // same construct. See pg. 218, lines 15-16.
#pragma omp target teams distribute num_teams(10) shared(share, num_teams) map(to: a[0:SIZE]) defaultmap(tofrom:scalar)
  for (int x = 0; x < SIZE; ++x) {
#pragma omp atomic write
    num_teams = omp_get_num_teams();
#pragma omp atomic
    share = share + a[x];
  }

  for (int x = 0; x < SIZE; ++x) {
    share = share - x;
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, (share != 0));
  OMPVV_ERROR_IF(errors != prev_errors, "The value of share is = %d", share);
  prev_errors = errors;

  share = 5;

#pragma omp target data map(tofrom: a[0:SIZE]) map(tofrom: share)
  {
#pragma omp target teams distribute num_teams(10) shared(share)
    for (int x = 0; x < SIZE; ++x) {
      a[x] = a[x] + share;
    }
  }

  for (int x = 0; x < SIZE; ++x) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, (a[x] - 5 != x));
  }

  if (num_teams == 1) {
    OMPVV_WARNING("Test operated on one team, results of default shared test are inconclusive.");
  }

  OMPVV_REPORT_AND_RETURN(errors);
}
