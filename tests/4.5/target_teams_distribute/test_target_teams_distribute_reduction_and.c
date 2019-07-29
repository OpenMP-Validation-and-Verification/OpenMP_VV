//===--- test_target_teams_distribute_reduction_and.c------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the reduction clause on a target teams distribute directive,
// testing that the variable in the reduction clause is properly reduced using
// the and operator.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int test_and(){
  char a[N];
  char result;
  char host_result;
  double false_margin = pow(exp(1), log(.5)/N);
  int errors = 0;
  int num_teams[N];
  int warned = 0;
  srand(1);

  for (int itr_count = 0; itr_count < 16; ++itr_count) {
    for (int x = 0; x < N; ++x) {
      if (rand() / (double)(RAND_MAX) < false_margin) {
	a[x] = 1;
      } else {
	a[x] = 0;
      }
      num_teams[x] = -x;
    }

    result = 1;
    host_result = 1;

#pragma omp target data map(to: a[0:N]) map(tofrom: num_teams[0:N])
    {
#pragma omp target teams distribute reduction(&&:result) map(alloc: a[0:N], num_teams[0:N])
      for (int x = 0; x < N; ++x) {
	num_teams[x] = omp_get_num_teams();
	result = result && a[x];
      }
    }

    for (int x = 0; x < N; ++x) {
      host_result = host_result && a[x];
    }

    if (itr_count == 0) {
      for (int x = 1; x < N; ++x) {
	if (num_teams[x-1] != num_teams[x]) {
	  OMPVV_WARNING("Kernel reported multiple numbers of teams.  Validity of testing of reduction clause cannot be guarunteed.");
	  warned += 1;
	}
      }
      if ((num_teams[0] == 1) && (warned == 0)) {
	OMPVV_WARNING("Test operated with one team.  Reduction clause cannot be tested.");
      } else if ((num_teams[0] <= 0) && (warned == 0)) {
	OMPVV_WARNING("Test reported invalid number of teams.  Validity of testing of reduction clause cannot be guarunteed.")
          }
    }
        
    OMPVV_TEST_AND_SET_VERBOSE(errors, host_result != result);
    if (host_result != result) {
      break;
    }
  }
  return errors;
}

int main() {
  int total_errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_and() !=0);
  OMPVV_REPORT_AND_RETURN(total_errors);
}
