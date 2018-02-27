// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

//===---- test_target_if.c -  --------------------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// The if clause determines if the section should be executed in the host or
// the device. There are three things to test here:
// (a) with offloading when 'if' clause evaluates to true then code
// be executed on the device
// (b) with offloading when 'if' clause evaluates to false then code should
// be executed on the host
// (c) without offloading all the code should be executed on the device
// The if clause is evaluated on runtime which means that variables could
// determine this behavior. We use a SIZE_THRESHOLD variable to check if we
// should execute on the device or the host. Before starting the test we
// sample offloading to see if it was enabled or not. If the code is executed
// in the device, the result should be c[i] = a[i] + b[i] = i + 1.
// If the code is executed on the host the result should be c[i] = -1
//
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define SIZE_THRESHOLD 512

// Test for OpenMP 4.5 target data with if
int main() {
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  int a[1024];
  int b[1024];
  int c[1024];
  int d[1024];
  int privatized_array[10];
  int privatized;
  int ishost;
  int errors[2] = {0,0};

  // a and b array initialization
  for (int x = 0; x < 1024; ++x) {
      a[x] = 1;
      b[x] = x;
      c[x] = 2*x;
      d[x] = 0;
  }

  for (int x = 0; x < 10; ++x){
      privatized_array[x] = x;
  }

  //Test privitization of data in firstprivate clause
  #pragma omp target data map(from: d[0:1024]) map(to: a[0:1024], b[0:1024], c[0:1024])
  {
      #pragma omp target teams distribute firstprivate(privatized)
      for (int x = 0; x < 1024; ++x){
          privatized = a[x] + b[x];
          d[x] = c[x] * privatized;
      }
  }

  for (int x = 0; x < 1024; ++x){
      if (d[x] != (1 + x)*2*x){
          if (isOffloading){
              errors[0] += 1;
          }
          else{
              errors[1] += 1;
          }
      }
  }
  //Test initialization of data in firstprivate clause
  #pragma omp target data map(from: d[0:1024]) map(to: a[0:1024], b[0:1024], c[0:1024])
  {
      #pragma omp target teams distribute firstprivate(privatized_array[0:10])
      for (int x = 0; x < 1024; ++x){
          d[x] = a[x] + b[x] + c[x] + privatized_array[x%10];
      }
  }

  for (int x = 0; x < 1024; ++x){
      if (d[x] != 1 + 3 * x + (x%10)){
          if (isOffloading){
              errors[0] += 1;
          }
          else{
              errors[1] += 1;
          }
      }
  }

  if (!errors[0] && !errors[1]) {
    OMPVV_INFOMSG("Test passed with offloading %s", (isOffloading ? "enabled" : "disabled"));
  } else if (errors[0]==0 && errors[1]!=0) {
    OMPVV_ERROR("Test failed on host with offloading %s.", (isOffloading ? "enabled" : "disabled"));
  } else if (errors[0]!=0 && errors[1]==0) {
    OMPVV_ERROR("Test failed on device with offloading %s.", (isOffloading ? "enabled" : "disabled"));
  } else if (errors[0]!=0 && errors[1]!=0) {
    OMPVV_ERROR("Test failed on host and device with offloading %s.", (isOffloading ? "enabled" : "disabled"));
  }

  OMPVV_REPORT_AND_RETURN((errors[0] + errors[1]));
}
