//===--- test_target_teams_distribute_num_teams.c----------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the num_teams clause on a target teams distribute directive to
// indicate a requested number of teams to execute the teams distribute region.
// The specifications indicate that the number of teams that are given can be any
// number that is equal to or less than the indicated value.  We first run a
// target teams distribute region without the clause to see what the default
// number of teams is, and then we use a value that is less than that in the
// test of the num_teams clause.  If the region is run with more teams than
// indicated, the test errors.  If the region is run with less teams than
// indicated, the test issues a warning since it is known that the device can
// run with more teams than was actually given.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define SIZE_THRESHOLD 512

int main() {
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  int a[1024];
  int b[1024];
  int c[1024];
  int default_num_teams = 0;
  int num_teams = 0;
  int errors = 0;

  for (int x = 0; x < 1024; ++x) {
      a[x] = 1;
      b[x] = x;
      c[x] = 0;
  }

  #pragma omp target data map(tofrom: default_num_teams)
  {
      #pragma omp target teams distribute map(tofrom: default_num_teams, c[0:1024]) map(to: a[0:1024], b[0:1024])
      for (int x = 0; x < 1024; ++x){
          default_num_teams = omp_get_num_teams();
          c[x] = a[x] + b[x];
      }
  }

  if (default_num_teams == 1){
      OMPVV_WARNING("Test operated with one team.  Testing num_teams clause cannot be done.");
  }
  else if(default_num_teams <= 0){
      OMPVV_ERROR("Test returned num_teams <= 0.  Maybe omp_get_num_teams() is not returning correct number of teams.");
      errors = 1;
  }
  else{
      #pragma omp target data map(to: a[0:1024], b[0:1024]) map(from: c[0:1024], num_teams)
      {
          #pragma omp target teams distribute num_teams(default_num_teams - 1) map(alloc: num_teams)
          for (int x = 0; x < 1024; ++x){
              num_teams = omp_get_num_teams();
              c[x] = a[x] + b[x];
          }
      }
      if (num_teams > default_num_teams - 1){
          errors += 1;
          OMPVV_ERROR("Test ran on more teams than requested.");
          return errors;
      }
      else if (num_teams < default_num_teams - 1){
          OMPVV_WARNING("Test ran on less teams than requested.  Still spec-conformant.");
      }
      else{
          OMPVV_INFOMSG("Test passed with offloading %s", (isOffloading ? "enabled" : "disabled"));
      }
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
