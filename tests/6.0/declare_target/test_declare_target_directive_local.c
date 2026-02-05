//----------------test_declare_target_directive_local.c--------------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 902, line 4
// ***********
// DIRECTIVE: declare_target
// CLAUSE: local
// The declare_target directive is used with the local clause to make data
// persist on multiple devices, each with their own copy of the referenced
// variables in the clause. If the values retrieved from each of the available
// devices add up to the expected amount, the test will pass.
//-------------------------------------------------------------------------------------//

#include "ompvv.h"
#include <omp.h>

#define N 100

int sum;
int x[N];

#pragma omp declare_target local(sum, x)
#pragma omp begin declare_target
void init_x(int dev_id) {
  for (int j = 0; j < N; ++j) {
    x[j] = j + dev_id;
  }
}

void foo(void) {
  #pragma omp for reduction(+ : sum)
  for (int i = 0; i < N; i++) {
    sum += x[i];
  }
}
#pragma omp end declare target

int test_declare_target_local() {
  int errors = 0;
  int TotGpus = omp_get_num_devices();
  int errors_arr[TotGpus];

  for (int i = 0; i < TotGpus; i++) {
    errors_arr[i] = 0;
    {
      init_x(i);
      sum = 0;
    }
  }
  for (int i = 0; i < TotGpus; i++) {
    #pragma omp target parallel device(i)
    {
      foo();
    }
  }
  #pragma omp taskwait

  for (int i = 0; i < TotGpus; i++) {
    #pragma omp target map(tofrom : errors_arr[i]) device(i)
    {
      if ((N) * (N - 1) / 2 + (N)*i != sum) {
        ++errors_arr[i];
      }
    }
  }
  for (int i = 0; i < TotGpus; i++) {
    errors += errors_arr[i];
  }
  OMPVV_TEST_AND_SET(errors, errors != 0);

  return errors;
}

int main() {
  int errors = 0;
  int TotGpus = omp_get_num_devices();
  OMPVV_WARNING_IF(TotGpus < 1, "Test requires non-host devices, but none were "
                                "found. \n This test will be skipped.\n");
  OMPVV_CHECK_TO_SKIP(TotGpus < 1)
  OMPVV_TEST_OFFLOADING;

  OMPVV_TEST_AND_SET(errors, test_declare_target_local() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
