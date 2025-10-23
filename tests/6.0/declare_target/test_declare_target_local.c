//--------------- test_declare_target_local.c -------------------------------//
// OpenMP API Version 6.0 November 2024
/////////////
// Pg. 902, line 4
// ***********
// DIRECTIVE: declare_target
// CLAUSE: local
// The target_data directive is being used in three scenaries of various task
// configurations for the same functional task. Because target_data is a
// task-generating composite construct, it will have expected effects that are
// being tested in each scenario. If the work_function executes correctly, then
// the value of a should expectedly change to 1.
//-------------------------------------------------------------------------------------//

#include "ompvv.h"
#include <omp.h>

#define N 100

// int work_function(int var) { return var + 1; }
// void check_function(int var, int *errors) {
//   OMPVV_ERROR_IF(var != 1, "Expected %i, received %i", 1, var);
//   OMPVV_TEST_AND_SET(*errors, var != 1);
//   return;
// }
  int sum;
  int x[N];

  #pragma omp declare_target to(sum, x)//local(sum, x)
  #pragma omp begin declare_target
  void init_x(int dev_id) {
    for (int j = 0; j < N; ++j) {
        x[j] = j + dev_id;
    }
  }
  void foo(void) {
    int i;
    #pragma omp for reduction(+:sum)
    for (i = 0; i < N; i++) {
        sum += x[i];
    }
  }
  #pragma omp end declare target


int test_declare_target_local() {
  int errors = 0;

  int TotGpus = omp_get_num_devices();
  OMPVV_WARNING_IF(TotGpus < 1, "Test requires non-host devices, but none were found. \n This test will be skipped.\n");
  if (TotGpus < 1) {
    return OMPVV_SKIPPED_EXIT_CODE;
  }

int errors_arr[TotGpus] = {0};
  for (int i = 0; i < TotGpus; i++) {
      #pragma omp target device(i) //map(tofrom: sum)
      {
        init_x(i);
        sum = 0;
      }
  }

  for (int i = 0; i < TotGpus; i++) {
    #pragma omp target parallel device(i) nowait //map(tofrom: sum) 
    {
      foo();
    }
  }
  #pragma omp taskwait

  for (int i = 0; i < TotGpus; i++) {
      #pragma omp target map(tofrom: errors_arr) device(i) //map(tofrom: sum)
      {
      //printf("sum: %d, expected: %d\n", sum, (N)*(N-1)/2 + (N)*i);
        
      if ((N)*(N-1)/2 + (N)*i != sum){
        ++errors_arr[i];
      }
      /*
      for (int j=0; j < N; ++j){
        printf("index %d, value %d\n", j, x[j]);
      }
      */
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
  OMPVV_TEST_OFFLOADING;

  if(test_exit_code == OMPVV_SKIPPED_EXIT_CODE)
    { OMPVV_REPORT_AND_RETURN(test_exit_code) }

  OMPVV_TEST_AND_SET(errors, test_declare_target_local() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
