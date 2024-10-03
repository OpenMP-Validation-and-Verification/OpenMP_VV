//--------------- test_fuse_looprange.c------------------------------------//
// OpenMP API Version 6.0 August 2024
// Pg. 868, line 16
// ***********
// DIRECTIVE:fuse
// CLAUSE:looprange
// ***********
// The severity clause for the parallel directive is being tested. If the clause
// executes correctly, than the user will see a message corresponding to the top
// parallel region. Otherwise, the user should notice a message corresponding to
// the second region. Additionally, the return value of the test function should
// be non-zero.
//----------------------------------------------------------------------------//
#include "ompvv.h"

#define N 10
int test_looprange() {
  int return_value = 1;
  int A[N][N] = {0};
  int sum = 0;

  #pragma omp fuse looprange(1, 1)
  for (int i = 0; i < N; ++i){
    for (int j = 0; j < N; ++j){
      A[i][j] = i*(N) + j + 1;
    }
  }

  for (int i = 0; i < N; ++i){
    for (int j = 0; j < N; ++j){
      sum += A[i][j];
    }
  }

  if (sum != (N*N*(N*N-1))/2)
    return 1;
  return 0;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET(errors, test_looprange() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}
