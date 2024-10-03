//--------------- test_fuse_looprange.c---------------------------------------//
// OpenMP API Version 6.0 August 2024
// Pg. 868, line 16
// ***********
// DIRECTIVE:fuse
// CLAUSE:looprange
// ***********
// The looprange clause for the fuse directive is being tested. The clause takes
// the 4 loops and merges every 2 loops (right count) so that the total number
// of loops is 2 (left count). The entries of the 4 * N matrix will be from 1 to
// 4 * N. If an entry is not entered correctly, then errors will be incremented.
//----------------------------------------------------------------------------//
#include "ompvv.h"

#define N 4

int test_looprange() {
  int A[4][N] = {0};
  int errors = 0;
  int j;

#pragma omp fuse looprange(2, 2)
  {
    for (j = 0; j < N; ++j)
      A[0][j] = j + 1;
    for (j = 0; j < N; ++j)
      A[1][j] = N + j + 1;
    for (j = 0; j < N; ++j)
      A[2][j] = 2 * N + j + 1;
    for (j = 0; j < N; ++j)
      A[3][j] = 3 * N + j + 1;
  }

  for (int i = 0; i < 4; ++i) {
    for (j = 0; j < N; ++j) {
      if (A[i][j] != i * N + j + 1)
        ++errors;
    }
  }

  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET(errors, test_looprange() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}
