// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define N 1000

/*
 * Test if it is possible to:
 * 1. target enter data to depend 'in' and 'out'
 * 2. target exit data to depend 'in' and 'out'
 * 3. Mix target-based tasks with host tasks.
 */
int test_async_between_task_target() {
    puts("test_async_between_task_target");

  int errors = 0;
  bool isHost = true;
  double sum = 0.0;
  double* h_array = (double *) malloc(N * sizeof(double));
  double* in_1 = (double *) malloc(N * sizeof(double));
  double* in_2 = (double *) malloc(N * sizeof(double));
  
  // host task
#pragma omp task depend(out: in_1) shared(in_1)
  {
    for (int i = 0; i < N; ++i) {
      in_1[i] = 1;
    }
  }

  // host task
#pragma omp task depend(out: in_2) shared(in_2)
  {
    for (int i = 0; i < N; ++i) {
      in_2[i] = 2;
    }
  }

  // target enter data
#pragma omp target enter data map(alloc: h_array[0:N]) map(to: in_1[0:N]) map(to: in_2[0:N]) depend(out: h_array) depend(in: in_1) depend(in: in_2) 

  // target task to compute on the device
  // adding redundant depends on in_1 + in_2 to make the test work if compiled for the host
#pragma omp task shared (isHost, h_array, in_1, in_2) depend(inout: h_array) depend(in: in_1) depend(in: in_2)
  {
#pragma omp target map(tofrom: isHost) 
    {
      isHost = omp_is_initial_device();
      for (int i = 0; i < N; ++i) {
        h_array[i] = in_1[i]*in_2[i];
      }
    }
  }

  // target exit data
#pragma omp target exit data map(from: h_array[0:N]) depend(inout: h_array) 

  // host task
#pragma omp task depend(in: h_array) shared(sum, h_array)
  {
    // checking results
    for (int i = 0; i < N; ++i) {
      sum += h_array[i];
    }
  }
#pragma omp taskwait

  errors = 2.0*N != sum;
  
  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s: sum = %g\n", (isHost ? "host" : "device"), sum);

  return errors;
}

/*
 * Test if it is possible to:
 * 1. target enter data to depend 'out'
 * 2. target exit data to depend 'in'
 * 3. use nowait for async
 */
int test_async_between_target() {
  puts("test_async_between_target");

  int errors = 0;
  bool isHost = true;
  int sum = 0;
  int* h_array = (int *) malloc(N * sizeof(int));
  int val = 2;

  // target enter data
#pragma omp target enter data map(alloc: h_array[0:N]) depend(out: h_array) 

#pragma omp target enter data map(to: val) depend(out: val) 

#pragma omp target map(tofrom: isHost) depend(inout: h_array) depend(in: val) 
  {
    isHost = omp_is_initial_device();
    for (int i = 0; i < N; ++i) {
      h_array[i] = val;
    }
  }

  // target exit data
#pragma omp target exit data map(from: h_array[0:N]) depend(in: h_array) 

#pragma omp taskwait

  // checking results
  for (int i = 0; i < N; ++i) {
    sum += h_array[i];
  }
  errors = 2*N != sum;

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s: sum = %d\n", (isHost ? "host" : "device"), sum);

  return errors;
}

int main(){
  int errors = 0;

  errors += test_async_between_target();
  errors += test_async_between_task_target();

  return errors;
}

