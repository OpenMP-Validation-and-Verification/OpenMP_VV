//===--- test_target_teams_distribute_reduction_bitand.c---------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the reduction clause on a target teams distribute directive,
// testing that the variable in the reduction clause is properly reduced using
// the bitand operator.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int test_bitand() {
  unsigned int a[N];
  double false_margin = pow(exp(1), log(.5)/N); // See the 'and' operator test for
  int errors = 0;                               // an exaplantion of this math.
  int num_teams[N];
  int warned = 0;
  srand(1);

  for (int x = 0; x < N; ++x) {
    for (int y = 0; y < 16; ++y) {
      if (rand() / (double) RAND_MAX < false_margin) {
	a[x] += 1 << y;
      }
    }
    num_teams[x] = -x;
  }

  unsigned int b = 0;
  for (int x = 0; x < 16; ++x) {
    b = b + (1 << x);
  }

#pragma omp target teams distribute reduction(&:b) defaultmap(tofrom:scalar)
  for (int x = 0; x < N; ++x) {
    num_teams[x] = omp_get_num_teams();
    b = b & a[x];
  }

  unsigned int host_b = a[0];

  for (int x = 0; x < N; ++x) {
    host_b = host_b & a[x];
  }

  for (int x = 1; x < N; ++x) {
    if (num_teams[x-1] != num_teams[x]) {
      OMPVV_WARNING("Kernel reported differing numbers of teams.  Validity of testing of reduction clause cannot be guaranteed.");
      warned += 1;
    }
  }
  if ((num_teams[0] == 1) && (warned == 0)) {
    OMPVV_WARNING("Test operated with one team.  Reduction clause cannot be tested.");
  } else if ((num_teams[0] <= 0) && (warned == 0)) {
    OMPVV_WARNING("Test reported invalid number of teams.  Validity of testing of reduction clause cannot be guaranteed.");
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, b != host_b);
  OMPVV_ERROR_IF(host_b != b, "Bit on device is %d but expected bit from host is %d.", b, host_b);

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;

  int total_errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_bitand() != 0);

  OMPVV_REPORT_AND_RETURN(total_errors);
}
