//===--- test_target_teams_distribute_nowait.c-------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the nowait clause on a target teams distribute directive and
// uses a barrier to resyncronize the target regions.  Since we can't be sure
// that operations will be asyncronous, we can only raise a warning if they are
// not executed asynchronously.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024
#define ITERATIONS 1024

int main() {
  OMPVV_WARNING("This test does not throw an error if tasks fail to execute asynchronously, as this is still correct behavior. If execution is not asynchronous, we will throw a warning.");
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  int a[N];
  int b[N];
  int c[N];
  int d[N];
  int e[N];
  int f[N];
  int errors = 0;
  int race_condition_found = 0;

  for (int y = 0; y < ITERATIONS; ++y) {
    for (int x = 0; x < N; ++x) {
      a[x] = x + y;
      b[x] = 2*x + y;
      c[x] = 0;
      d[x] = 3*x + y;
      e[x] = 4*x + y;
      f[x] = 0;
    }

#pragma omp target data map(to: a[0:N], b[0:N], d[0:N], e[0:N]) map(from: c[0:N], f[0:N])
    {
#pragma omp target teams distribute nowait map(alloc: a[0:N], b[0:N], c[0:N])
      for (int x = 0; x < N; ++x) {
	c[x] = a[x] + b[x];
      }
#pragma omp target teams distribute map(alloc: c[0:N], d[0:N], e[0:N], f[0:N])
      for (int x = 0; x < N; ++x) {
	f[x] = c[x] + d[x] + e[x];
      }
#pragma omp taskwait
    }

    for (int x = 0; x < N; ++x) {
      OMPVV_TEST_AND_SET_VERBOSE(errors, c[x] != 3*x + 2*y);
      if (f[x] != 10*x + 4*y) {
	race_condition_found = 1;
      }
    }
  }

  OMPVV_WARNING_IF(race_condition_found == 0, "Could not show that nowait had any effect on target teams distribute construct.");
  OMPVV_INFOMSG_IF(race_condition_found == 1, "At least one race condition was introduced, nowait had an effect.");

  OMPVV_REPORT_AND_RETURN(errors);
}
