//===---- test_dispatch_device.c -----------------------------------------===//
// 
// OpenMP API Version 5.1
//
// Uses dispatch construct as context for variant directive. Uses device
// clause on last device. While default device could be used, this would
// not change omp_get_default_device, so using the last device is more
// exhaustive. 
//
// Inspired by "OpenMP 5.1 Features: The Dispatch Construct" video:
// https://www.youtube.com/watch?v=ruugaX95gIs
// 
//===-------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ompvv.h"
#include <stdbool.h>

#define N 1024

int errors;
int i = 0;
int arr[N];
void add_dev(int *arr);

#pragma omp declare variant(add_dev) match(construct={dispatch}) 
void add(int *arr){
   #pragma omp parallel for
   for (int i = 0; i < N; i++){ // Base function adds 2 to array values
      arr[i] = arr[i]+1;
   }
}

void add_dev(int *arr){
   for (int i = 0; i < N; i++){
      arr[i] = arr[i]+2+omp_get_default_device(); // Variant function adds 4 to array values
   }
}

int test_wrapper() { 
   errors = 0;
   int device_num = omp_get_num_devices()-1; // last available device. 

   add(arr);
   for(i = 0; i < N; i++){
      OMPVV_TEST_AND_SET(errors, arr[i] != i+1);
   } 
   OMPVV_ERROR_IF(errors > 0, "Base function is not working properly");

   #pragma omp dispatch device(device_num)
      add(arr);
   
   for(i = 0; i < N; i++){
      OMPVV_TEST_AND_SET(errors, arr[i] != i+2+device_num);
   }
   OMPVV_ERROR_IF(errors > 0, "Dispatch is not working properly");

   return errors;
}

int main () {
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_wrapper());
   OMPVV_REPORT_AND_RETURN(errors);
}  