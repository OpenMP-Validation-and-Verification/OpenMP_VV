#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define ARRAY_SIZE 1024

// Test for OpenMP 4.5 target data with if
int main() {
  OMPVV_TEST_OFFLOADING;

  int a[ARRAY_SIZE];
  int b[ARRAY_SIZE];
  int num_teams = 0;
  int errors = 0;
  int is_host;

  // a and b array initialization
  for (int x = 0; x < ARRAY_SIZE; ++x) {
      a[x] = 1;
      b[x] = x;
  }

  #pragma omp target data map(tofrom: a[0:ARRAY_SIZE], num_teams, is_host) map(to: b[0:ARRAY_SIZE])
  {
      #pragma omp target teams distribute
      for (int x = 0; x < ARRAY_SIZE; ++x){
          is_host = omp_is_initial_device();
          num_teams = omp_get_num_teams();
          a[x] += b[x];
      }
  }

  for (int x = 0; x < ARRAY_SIZE; ++x){
      OMPVV_TEST_AND_SET(errors, (a[x] != 1 + b[x]));
  }


  if (num_teams == 1)
      OMPVV_WARNING("Test operated with one team.  Parallelism of teams distribute can't be guarunteed.");

  OMPVV_REPORT_AND_RETURN(errors);
}
