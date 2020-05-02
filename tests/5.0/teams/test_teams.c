//===--- test_teams.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test uses the teams directive on host and verifies of the 
// requested number of teams with requested number of threads were 
// created. 
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 4

int main() {
  int num_teams[N];
  int num_threads[N];
  int errors[2] = {0,0};
  int is_offloading;


  for (int x = 0; x < N; ++x) {
    num_teams[x] = -99;
    num_threads[x] = -99;
  }

#pragma omp teams num_teams(N) thread_limit(N)
{
    num_teams[omp_get_team_num()] = omp_get_num_teams();
    num_threads[omp_get_team_num()]= omp_get_num_threads();

}

  if (num_teams[0] == 1) {
    OMPVV_WARNING("Test operated with one team. num_teams requested were %d.", N);
  } else if (num_teams[0] < 1) {
    OMPVV_ERROR("omp_get_num_teams() reported a value less than one.");
  }

  if (num_threads[0] == 1) {
    OMPVV_WARNING("Team 0 reported only 1 thread. thread_limit was set to %d.",N);
  } else if (num_threads[0] < 1) {
    OMPVV_ERROR("omp_get_num_threads() reported a value below one.");
  }
  for (int x = 1; x < num_teams[0]; ++x) {
    if (num_teams[x] != num_teams[x - 1]) {
      errors[0]++;
    }
    if (num_threads[x] != num_threads[x - 1]) {
      errors[1]++;
    }
  }

  OMPVV_INFOMSG_IF(!(errors[0] + errors[1]), "Test passed with %d teams and %d threads per team.", num_teams[0], num_threads[0]);

  OMPVV_REPORT_AND_RETURN(errors[0] + errors[1]);
}
