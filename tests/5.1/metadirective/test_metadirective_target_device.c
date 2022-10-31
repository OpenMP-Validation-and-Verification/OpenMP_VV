//===--------------------- test_metadirective_target_device.c ---------------------===//
//
// OpenMP API Version 5.1 Nov 2020
// 
// Tests that target_device is recognized & will work properly when matching an nvidia
// GPU.
//
////===---------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_metadirective_target_device() {
   int errors = 0;
   int A[N];

   for (int i = 0; i < N; i++) {
      A[i] = 1;
   }

   #pragma omp target map(tofrom: A)
   {
      #pragma omp parallel num_threads(4)
      {
      // We expect at least one of these when conditons to eval to true, thus having the nothing directive utilized
      #pragma omp metadirective \
         when( target_device={kind(nohost)}: masked ) \
         when( target_device={arch("nvptx")}: masked ) \
         when( implementation={vendor(amd)}: masked ) \
         default( for)
            for (int i = 0; i < N; i++) {
               A[i] = omp_get_thread_num();
            }
      }
   }

   for (int i = 0; i < N; i++) {
      OMPVV_TEST_AND_SET(errors, A[i] != 0);
   }

   OMPVV_INFOMSG("Test ran with a number of available devices greater than 0");

   return errors;
}

int main () {
  int errors = 0;
  OMPVV_TEST_OFFLOADING;

  if (omp_get_num_devices() > 0) {
    errors = test_metadirective_target_device();
  } else {
    OMPVV_INFOMSG("Cannot test target_device without a target device!")
  }

  OMPVV_REPORT_AND_RETURN(errors);

  return 0;
}
