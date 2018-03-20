#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define SIZE_THRESHOLD 512

// Test for OpenMP 4.5 target data with if
int main() {
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  int a[1024];
  int b[1024];
  int num_teams = 0;
  int errors = 0;
  int is_host;

  // a and b array initialization
  for (int x = 0; x < 1024; ++x) {
      a[x] = 1;
      b[x] = x;
  }

  #pragma omp target data map(tofrom: a[0:1024], num_teams, is_host) map(to: b[0:1024])
  {
      #pragma omp target teams distribute
      for (int x = 0; x < 1024; ++x){
          is_host = omp_is_initial_device();
          num_teams = omp_get_num_teams();
          a[x] += b[x];
      }
  }

  for (int x = 0; x < 1024; ++x){
      OMPVV_TEST_AND_SET(errors, (a[x] != 1 + b[x]));
  }


  if (!errors) {
    OMPVV_INFOMSG("Test passed with offloading %s", (isOffloading ? "enabled" : "disabled"));
    if (num_teams == 1){
        OMPVV_WARNING("Test operated with one team.  Parallelism of teams distribute can't be guarunteed.");
    }
  } else if (!is_host) {
    OMPVV_ERROR("Test failed on device with offloading %s.", (isOffloading ? "enabled" : "disabled"));
  } else if (is_host) {
    OMPVV_ERROR("Test failed on host with offloading %s.", (isOffloading ? "enabled" : "disabled"));
  }

  OMPVV_REPORT_AND_RETURN(errors);
}
