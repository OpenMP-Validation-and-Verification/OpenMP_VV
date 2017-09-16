// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define N 1000

void init_1d(int* a);

// Test for OpenMP 4.5 target data map with array 1d section [lower:]
int fail_on_map() {
  int errors = 0, isHost = 0;

  int a1d[N];
  int a1d2[N];
  int a1d3[N];
  int a1d4[N];
  init_1d(a1d);
  init_1d(a1d2);
  init_1d(a1d3);
  init_1d(a1d4);

  // OpenMP API - V4.5 Nov2015. 2.4. Array sections, page 45 line 13:
  // When the length is absent, it defaults to the size of the array
  // dimension minus the lower-bound.
  // This default mapping is failing. It seems like it is not substracting
  // the lower-bound. a1d[1:N-1] works fine
#pragma omp target map(tofrom: isHost) map(tofrom: a1d[N/2:], a1d2[:N/2], a1d3[N/2:], a1d4[:N/2])
  {
    isHost = omp_is_initial_device();
    for (int i = 0; i < N/2; ++i){
      a1d[N/2+i] = 1;
      a1d2[i] = 2;
      a1d3[N/2+i] = 3;
      a1d4[i] = 4;
    }
  } // end target

  // checking errors 
  for (int i = 0; i < N; ++i) {
    if (i < N/2) {
      errors += a1d[i] == 0 ? 0 : 1;
      errors += a1d2[i] == 2 ? 0 : 1;
      errors += a1d3[i] == 0 ? 0 : 1;
      errors += a1d4[i] == 4 ? 0 : 1;
    }
    else {
      errors += a1d[i] == 1 ? 0 : 1;
      errors += a1d2[i] == 0 ? 0 : 1;
      errors += a1d3[i] == 3 ? 0 : 1;
      errors += a1d4[i] == 0 ? 0 : 1;
    }
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}

int main() {

  int errors = 0;
  
  errors = fail_on_map();

  return errors;
}

void init_1d(int* a) {
  for (int i = 0; i < N; ++i)
    a[i] = 0;
}
