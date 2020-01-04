//===--- test_target_teams_distribute_depend.c-------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test defines a series of functions that enumerate the possible
// combinations of the interactions of the depends clause with the various
// dependence-types: in, out, inout.  With each combination, it tests if
// the dependence between them (if necessary) is forced.  If there is no
// required dependence, then the test tries to see if race conditions between
// the two independent target regions can be formed.  However, if it fails
// to do so, it only issues a warning as this is both a imperfect test of
// the independence and it is not requried that they both execute at the
// same time.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_target_teams_distribute_depend_in_in() {
  int isOffloading = 0;
  int a[N];
  int b[N];
  int c[N];
  int all_valid = 1;
  int race_found = 0;

  for (int x = 0; x < N; ++x) {
    a[x] = x;
    b[x] = 2 * x;
    c[x] = 0;
  }

#pragma omp target data map(to: a[0:N], b[0:N]) map(tofrom: c[0:N])
  {
#pragma omp target teams distribute nowait depend(in: c) map(alloc: a[0:N], b[0:N], c[0:N])
    for (int x = 0; x < N; ++x) {
      c[x] += a[x] + b[x];
    }
#pragma omp target teams distribute nowait depend(in: c) map(alloc: a[0:N], b[0:N], c[0:N])
    for (int x = 0; x < N; ++x) {
      c[x] += 2 * (a[x] + b[x]);
    }
  }

  for (int x = 0; x < N; ++x) {
    if (!(c[x] == 3 * x || c[x] == 6 * x || c[x] == 9 * x)) {
      all_valid = 0;
    }
    if (c[x] == 3 * x || c[x] == 6 * x) {
      race_found = 1;
    }
  }

  if (!(all_valid == 1 && race_found == 1)) {
    OMPVV_WARNING("Test could not prove asyncronous operations of depend(in) task with other depend(in) task with offloading %s", (isOffloading ? "enabled" : "disabled"));
  }
  return 0;
}
int main() {
  int errors = 0;
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  errors += test_target_teams_distribute_depend_in_in();
  if (errors != 0) {
    OMPVV_INFOMSG("Test passed with offloading %s", (isOffloading ? "enabled" : "disabled"));
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
