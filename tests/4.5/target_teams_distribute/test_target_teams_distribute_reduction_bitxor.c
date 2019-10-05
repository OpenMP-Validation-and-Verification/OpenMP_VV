//===--- test_target_teams_distribute_reduction_bitxor.c----------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the reduction clause on a target teams distribute directive,
// testing that the variable in the reduction clause is properly reduced using
// the bitxor operator.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int test_bitxor() {
  unsigned int a[N];
  int errors = 0;
  int num_teams[N];
  int warned = 0;
  srand(1);

  for (int x = 0; x < N; ++x) {
    a[x] = (unsigned int) rand() / (double) (RAND_MAX / 2);
    num_teams[x] = -x;
  }

  unsigned int b = 0;

#pragma omp target teams distribute reduction(^:b) map(to: a[0:N]) map(tofrom: b, num_teams[0:N])
  for (int x = 0; x < N; ++x) {
    num_teams[x] = omp_get_num_teams();
    b = (b ^ a[x]);
  }

  unsigned int host_b = 0;

  for (int x = 0; x < N; ++x) {
    host_b = (host_b ^ a[x]);
  }

  for (int x = 1; x < N; ++x) {
    if (num_teams[x-1] != num_teams[x]) {
      OMPVV_WARNING("Kernel reported multiple numbers of teams.  Validity of testing of reduction clause cannot be guaranteed.");
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

  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_bitxor() != 0);

  OMPVV_REPORT_AND_RETURN(total_errors);
}
