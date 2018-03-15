#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

// Test for OpenMP 4.5 target data with if
int main() {
  int a[1024];
  int b[1024];

  // a and b array initialization
  for (int x = 0; x < 1024; ++x) {
      a[x] = x;
      b[x] = 0;
  }

  #pragma omp target enter data map(to: a[0:1024])

  #pragma omp target teams distribute map(delete: a[0:1024]) // Release also has issues
  for (int x = 0; x < 1024; ++x){
      b[x] = a[x];
  }

  return 0;
}
