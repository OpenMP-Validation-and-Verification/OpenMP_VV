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
  
  if (!isOffloading)
  OMPVV_WARNING("It is not possible to test conditional data transfers "
                 "if the environment is shared or offloading is off. Not testing "
                 "anything")
  int a[1024];
  int b[1024];
  int c[1024];
  int size, i = 0, errors[2] = {0,0}, isHost = -1;

  // a and b array initialization
  for (i = 0; i < 1024; i++) {
    a[i] = 1;
    b[i] = i;
  }

  // check multiple sizes. 
  for (size = 256; size <= 1024; size += 256) {
    // C initialization
    for (i = 0; i < size; i++) {
      c[i] = -1;
    }
#pragma omp target if(size > SIZE_THRESHOLD) map(to: size)  \
        map(tofrom: c[0:size])                                       \
        map(to: a[0:size], b[0:size])  map(tofrom: isHost)
    {
      isHost = omp_is_initial_device();
      int alpha = (isHost ? 0 : 1);
      int j = 0;
      for (j = 0; j < size; j++) {
        // c[j] is zero if executed in the host
        // c[j] is 1+j if executed on the device
        c[j] = alpha*(a[j] + b[j]);
      }
    } // end target

    // checking results 
    for (i = 0; i < size; i++) {
      if (isOffloading && size > SIZE_THRESHOLD) {
        // Should have executed on the device
        // if offloading was used
        // c[i] is zero if it was executed in the host
        OMPVV_TEST_AND_SET(errors[0], (c[i] != i + 1));//error when executed on the device
      } else {
        // Should have executed in the host
        // with or without offloading
        OMPVV_TEST_AND_SET(errors[1], (c[i] != 0));
      } //end-else 
    }
  } // end-for size

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
