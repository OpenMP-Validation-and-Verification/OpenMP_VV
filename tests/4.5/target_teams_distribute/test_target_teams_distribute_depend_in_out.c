//===--- test_target_teams_distribute_depend_in_out.c-------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test checks in-out and in-inout dependency by checking order-dependent
// results from pairs of possibly asynchronous loops. The test fails if either 
// in-out or in-inout dependency is broken.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_target_teams_distribute_depend_in_out() {
  int isOffloading = 0;
  int a[N];
  int b[N];
  int c[N];
  int d[N];
  int errors = 0;

  for (int x = 0; x < N; ++x) {
    a[x] = x;
    b[x] = 2 * x;
    c[x] = 0;
    d[x] = 0;
  }

#pragma omp target data map(to: a[0:N], b[0:N]) map(alloc: c[0:N]) map(from: d[0:N])
  {
#pragma omp target teams distribute nowait depend(in: c) map(alloc: a[0:N], b[0:N], c[0:N])
    for (int x = 0; x < N; ++x) {
      c[x] = a[x] + b[x];
    }
#pragma omp target teams distribute nowait depend(out: c) map(alloc: b[0:N], c[0:N], d[0:N])
    for (int x = 0; x < N; ++x) {
      d[x] = c[x] + b[x];
    }
  }

  for (int x = 0; x < N; ++x) {
    if (d[x] != 5 * x) {
      errors = 1;
      break;
    }
  }

#pragma omp target data map(to: a[0:N], b[0:N]) map(alloc: c[0:N]) map(from: d[0:N])
  {
#pragma omp target teams distribute nowait depend(in: c) map(alloc: a[0:N], b[0:N], c[0:N])
    for (int x = 0; x < N; ++x) {
      c[x] = a[x] + b[x];
    }
#pragma omp target teams distribute nowait depend(inout: c) map(alloc: a[0:N], c[0:N], d[0:N])
    for (int x = 0; x < N; ++x) {
      d[x] = c[x] + a[x];
    }
  }

  for (int x = 0; x < N; ++x) {
    if (d[x] != 4 * x) {
      if (errors == 1) {
	OMPVV_ERROR("Test of depend(inout/out) task becoming dependent task of depend(in) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
	return 1;
      }
      else {
	OMPVV_ERROR("Test of depend(inout) task becoming dependent task of depend(in) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
	return 1;
      }
    }
  }
  if (errors == 1) {
    OMPVV_ERROR("Test of depend(out) task becoming dependent task of depend(in) task did not pass with offloading %s", (isOffloading ? "enabled" : "disabled"));
    return 1;
  }
  else {
    return 0;
  }
}

int main() {
  int errors = 0;
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  errors += test_target_teams_distribute_depend_in_out();
  OMPVV_INFOMSG_IF(errors != 0, "Test passed with offloading %s", (isOffloading ? "enabled" : "disabled"));
  OMPVV_REPORT_AND_RETURN(errors);
}
