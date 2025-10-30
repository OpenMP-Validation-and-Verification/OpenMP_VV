//--------------- test_declare_target_directive_local.c--------------------------------//
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
int sum_array[10];

#pragma omp declare_target to(sum, x, sum_array) // local(sum, x)
#pragma omp begin declare_target
// void init_x(int dev_id) {
//   for (int j = 0; j < N; ++j) {
//       x[j] = j + dev_id;
//   }
// }
void foo(int dev_id) {
  int i;
  for (int j = 0; j < N; ++j) {
    x[j] = j + dev_id;
  }
  #pragma omp for reduction(+ : sum)
  for (i = 0; i < N; i++) {
    //sum += x[i];
    sum_array[dev_id] += x[i];
  }
}
#pragma omp end declare target

int test_declare_target_local() {
  int errors = 0;
  int TotGpus = omp_get_num_devices();
  int errors_arr[TotGpus];
  //int sum_array[TotGpus];

  for (int i = 0; i < TotGpus; i++) {
    sum_array[i] = 0;
    errors_arr[i] = 0;
    // #pragma omp target device(i) map(tofrom : sum_array)
    // {
    //   // init_x(i);
    //   // sum = 0;
    //   sum_array[i] = 0;
    // }
  }

  for (int i = 0; i < TotGpus; i++) {
    #pragma omp target parallel device(i) nowait
    {
      foo(i);
    }
  }
  #pragma omp taskwait

  for (int i = 0; i < TotGpus; i++) {
    #pragma omp target map(tofrom : errors_arr, sum_array) device(i)
    {
      printf("%d\n", sum_array[i]);
      if ((N) * (N - 1) / 2 + (N)*i != sum_array[i]) {
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
  int errors = 0, test_exit_code = 0;
  int TotGpus = omp_get_num_devices();
  OMPVV_WARNING_IF(TotGpus < 1, "Test requires non-host devices, but none were "
                                "found. \n This test will be skipped.\n");
  OMPVV_CHECK_TO_SKIP(TotGpus < 1)
  OMPVV_TEST_OFFLOADING;

  OMPVV_TEST_AND_SET(errors, test_declare_target_local() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
