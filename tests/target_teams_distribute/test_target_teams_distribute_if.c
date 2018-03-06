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
  int devtest = 1;
  int errors[2] = {0,0};


  #pragma omp target enter data map(to: devtest)
  #pragma omp target
  {
      devtest = 0;
  }
  // a and b array initialization
  for (int x = 0; x < 1024; ++x) {
      a[x] = 1;
      b[x] = x;
  }

  if (devtest == 1){
      //There is a separate memory device, full data environment tests can procede
      #pragma omp target data map(tofrom: a[0:1024]) map(to: b[0:1024])
      {
          #pragma omp target teams distribute if(b[0] > 1)
          for (int x = 0; x < 1024; ++x){
              a[x] += b[x] + devtest;
          }
      }

      for (int x = 0; x < 1024; ++x){
          OMPVV_TEST_AND_SET(errors[1], (a[x] != 1));
      }

      #pragma omp target data map(tofrom: a[0:1024]) map(to: b[0:1024])
      {
          #pragma omp target teams distribute if(b[0] < 1)
          for (int x = 0; x < 1024; ++x){
              a[x] += b[x] + devtest;
          }
      }

      for (int x = 0; x < 1024; ++x){
          OMPVV_TEST_AND_SET(errors[0], (a[x] != 1 + b[x]));
      }

      #pragma omp target data map(to: a[0:1024], b[0:1024])
      {
          #pragma omp target teams distribute if(b[0] > 1)
          for (int x = 0; x < 1024; ++x){
              a[x] += b[x] + devtest;
          }
      }

      for (int x = 0; x < 1024; ++x){
          OMPVV_TEST_AND_SET(errors[1], a[x] != 2 + 2 * b[x]);
      }
  }
  else{
      #pragma omp target data map(tofrom: a[0:1024]) map(to: b[0:1024])
      {
          #pragma omp target teams distribute if(b[0] > 1)
          for (int x = 0; x < 1024; ++x){
              a[x] += b[x] + devtest;
          }
      }

      for (int x = 0; x < 1024; ++x){
          OMPVV_TEST_AND_SET(errors[1], a[x] != b[x] + 1);
      }

      #pragma omp target data map(tofrom: a[0:1024]) map(to: b[0:1024])
      {
          #pragma omp target teams distribute if(b[0] < 1)
          for (int x = 0; x < 1024; ++x){
              a[x] += b[x] + devtest;
          }
      }

      for (int x = 0; x < 1024; ++x){
          OMPVV_TEST_AND_SET(errors[1], a[x] != 2 * b[x] + 1);
      }
  }

  #pragma omp target exit data map(delete: devtest)

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
