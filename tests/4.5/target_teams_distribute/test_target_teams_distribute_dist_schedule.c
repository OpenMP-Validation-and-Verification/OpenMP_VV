//===------ test_target_teams_distribute_dist_schedule.c ------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// This test checks that the dist_schedule clause (which must have kind 
// static) correctly causes CHUNK_SIZE iterations to be split among the
// number of teams the test is run with, in a round-robin faction in order
// of the team number, when a chunk size is given. The test also confirms
// that when no chunk size is given, that each team receives no more than
// one chunk.
// 
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024
#define CHUNK_SIZE 64

int test_dist_schedule() {
  int errors = 0;
  int num_teams_a, num_teams_b;
  int a[N];
  int b[N];
  int a_check[N];
  int b_check[N];

  for (int i = 0; i < N; ++i) {
    a[i] = -1;
    b[i] = -1;
  }

#pragma omp target teams distribute map(from: num_teams_a) map(tofrom: a[0:N]) dist_schedule(static, CHUNK_SIZE)
  for (int i = 0; i < N; ++i) {
    if (omp_get_team_num() == 0) {
      num_teams_a = omp_get_num_teams();
    }
    a[i] = omp_get_team_num();
  }

  for (int i = 0; i < N; ++i) {
    printf("%d ", a[i]);
  }
  printf("\n\n");

#pragma omp target teams distribute map(from: num_teams_b) map(tofrom: b[0:N]) dist_schedule(static)
  for (int i = 0; i < N; ++i) {
    if (omp_get_team_num() == 0) {
      num_teams_b = omp_get_num_teams();
    }
    b[i] = omp_get_team_num();
  }

  for (int i = 0; i < N; ++i) {
    printf("%d ", b[i]);
  }
  printf("\n");

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;

  OMPVV_TEST_SHARED_ENVIRONMENT;

  int errors = 0;

  errors = test_dist_schedule();

  OMPVV_REPORT_AND_RETURN(errors);
}
