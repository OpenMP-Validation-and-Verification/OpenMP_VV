//===---test_metadirective_arch_nvidia_or_amd.c ----------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
// 
// Test for metadirectives based on OpenMP 5.0 examples metadirective.1-3.c
//
////===---------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 100

int metadirective2() {

   int i, device_num, which_device;
   int a[N], total[N];
   int errors = 0;
 
   for (int i = 0; i < N; i++) {
      a[i] = 0;  
   }

   for (device_num = 0; device_num < omp_get_num_devices(); device_num++) {
      #pragma omp target device(device_num)
      #pragma omp metadirective \
                  when( implementation=vendor(nvidia): \
                        teams num_teams(512) thread_limit(32) ) \
                  when( implementation=vendor(amd): \
                        teams num_teams(512) thread_limit(64) ) \
                  default (teams)

      which_device = omp_is_initial_device();

      #pragma omp distribute parallel for
         for (i = 0; i < N; i++) {
            a[i] = i;
         }
   }

   OMPVV_WARNING_IF(which_device != 0, "NVIDIA and AMD architecture not available, ran on host");

   for (i = 0; i < N; i++) {
      OMPVV_TEST_AND_SET_VERBOSE(errors, a[i] != i); 
   }        

   return errors;

}

int main () {

   int errors = 0;
   
   OMPVV_TEST_OFFLOADING;

   OMPVV_TEST_AND_SET_VERBOSE(errors, metadirective2());

   OMPVV_REPORT_AND_RETURN(errors);

}
                   
