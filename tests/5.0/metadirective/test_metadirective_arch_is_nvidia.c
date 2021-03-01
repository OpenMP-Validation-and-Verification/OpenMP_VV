//===---test_metadirective_is_nvidia.c -------------------------------------===//
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

int metadirective1() {
   
   int v1[N], v2[N], v3[N];

   int which_device;
   int errors = 0;

   for(int i=0; i<N; i++) { 
      v1[i] = (i+1); 
      v2[i] = -(i+1); 
   }

   #pragma omp target map(to:v1,v2) map(from:v3) device(0)
   #pragma omp metadirective \
                   when(   device={arch("nvptx")}: teams loop) \
                   default(                     parallel loop)

      which_device = omp_is_initial_device(); 

      for (int i = 0; i < N; i++) {
         v3[i] = v1[i] * v2[i];
      }

   OMPVV_WARNING_IF(which_device != 0, "NVIDIA architecture appears to be unavailable, metadirective ran with the variant specified in the default clause");

   for (int i = 0; i < N; i++) {
      OMPVV_TEST_AND_SET_VERBOSE(errors, v3[i] != v1[i] * v2[i]);
   }

   return errors;
}

int main () {
   
   int errors = 0;
   OMPVV_TEST_OFFLOADING;
 
   OMPVV_TEST_AND_SET_VERBOSE(errors, metadirective1());
  
   OMPVV_REPORT_AND_RETURN(errors);

}
