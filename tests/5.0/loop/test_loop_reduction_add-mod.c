//===--- test_loop_reduction_add-mod.c ------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test uses the reduction clause on a loop directive, testing that the
// variable in the reduction clause is properly reduced using the add
// operator.
//
//===----------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
//#include <math.h>
//#define N 1024
int main() {
  int n = -32; 
  float f = -32.0;
  double d = -32.0;
  char c = '(';
  int thread_counts[8] = {0, 0, 0, 0, 0, 0, 0, 0};

#pragma omp parallel num_threads(OMPVV_NUM_THREADS_HOST)
  {
    #pragma omp loop reduction(+:n,f,d,c) 
    for(int i=0;i<64;i++){
      n = n + 1;
      f = f + 1.0;  
      d = d + 1.0;  
      c = c + 1;
    }

    #pragma omp for 
    for(int i=0;i<64;i++){
      thread_counts[omp_get_thread_num()] += 1;
    }
  }

  for(int i = 0; i < 8; i++){
    printf("%i,",thread_counts[i]);
  }
  printf("\n%i,%f,%f,%c\n",n,f,d,c);
  return 0;
}
//int test_add() {
//  int a[N], b[N], num_threads[N];
//  int total = 0, expect_total = 0, errors = 0;
//
//  for (int x = 0; x < N; ++x) {
//    a[x] = 1;
//    b[x] = x;
//    num_threads[x] = -1;
//  }
//
//#pragma omp parallel num_threads(OMPVV_NUM_THREADS_HOST)
//  {
//#pragma omp loop reduction(+:total)
//    for (int x = 0; x < N; ++x) {
//      total += a[x] + b[x];
//    }
//#pragma omp for
//    for (int x = 0; x < N; ++x) {
//      num_threads[x] = omp_get_num_threads();
//    }
//  }
//
//  for (int x = 0; x < N; ++x) {
//    expect_total += a[x] + b[x];
//  }
//
//  for (int x = 1; x < N; ++x) {
//    OMPVV_WARNING_IF(num_threads[x - 1] != num_threads[x], "Test reported differing numbers of threads.  Validity of testing of reduction clause cannot be guaranteed.");
//  }
//  OMPVV_WARNING_IF(num_threads[0] == 1, "Test operated with one thread.  Reduction clause cannot be tested.");
//  OMPVV_WARNING_IF(num_threads[0] <= 0, "Test reported invalid number of threads.  Validity of testing of reduction clause cannot be guaranteed.");
//
//  OMPVV_TEST_AND_SET_VERBOSE(errors, expect_total != total);
//  OMPVV_ERROR_IF(expect_total != total, "Total from loop directive is %d but expected total is %d.", total, expect_total);
//
//  return errors;
//}
//
//int main() {
//  int total_errors = 0;
//
//  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_add() != 0);
//
//  OMPVV_REPORT_AND_RETURN(total_errors);
//}
