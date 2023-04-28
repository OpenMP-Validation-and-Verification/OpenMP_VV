//===------test_target_teams_num_teams.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Description:
// This is a basic test to demonstrate target teams + num_teams
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024
#define NUM_TEAMS_TO_SET 32

int testNumTeamsDefaut() {
  int a[N], b[N], c[N];
  int errors = 0;
  int num_actual_teams = 0;
  // Data Inititalize
  for (int i = 0; i < N; i++) {
    a[i] = 2*i;  // Even
    b[i] = 2*i + 1;  // Odd
    c[i] = 0;
  }
  // Execute on target
#pragma omp target teams map(to: a[0:N], b[0:N]) map(from: c[0:N]) \
        map(tofrom: num_actual_teams) \
        num_teams(NUM_TEAMS_TO_SET)
  {
    num_actual_teams = omp_get_num_teams();
    for (int i = 0; i < N; i++) {
      c[i] = a[i] + b[i];
    }
  }
  // Validate
  OMPVV_TEST_AND_SET_VERBOSE(errors, (num_actual_teams > NUM_TEAMS_TO_SET));
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, c[i] != a[i] + b[i]);
  }
  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, testNumTeamsDefaut());
  OMPVV_REPORT_AND_RETURN(errors);
}
