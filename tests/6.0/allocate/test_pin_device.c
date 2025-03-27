//===--- test_pin_device.c --------------------===//
//
// OpenMP API Version 6.0
// Tests that the pin_device memory allocator trait works
// as intended.
//
//===----------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 1024

int test_pin_device() {
   int errors = 0;
   int target_device = 0;
   int verified_device = -1;
   
   int *arr;
   omp_alloctrait_t trait = { omp_atk_pin_device, target_device };
   omp_allocator_handle_t pin_dev_alloc = omp_init_allocator(omp_default_mem_space, 1, &trait);
   
   arr = (int*)omp_alloc(N*sizeof(int), pin_dev_alloc);
   if (arr == NULL) {
       OMPVV_ERROR("Allocation failed when using omp_alloc with pin_device trait");
       errors++;
       return errors;
   }
   
   for (int i = 0; i < N; i++) {
       arr[i] = 1;
   }
   
   #pragma omp target map(tofrom: arr[0:N], verified_device) device(target_device)
   {
       verified_device = omp_get_device_num();
       
       for (int i = 0; i < N; i++) {
           arr[i] = 2;
       }
   }
   
   OMPVV_TEST_AND_SET_VERBOSE(errors, verified_device != target_device);
   
   for (int i = 0; i < N; i++) {
       OMPVV_TEST_AND_SET_VERBOSE(errors, arr[i] != 2);
   }
   
   omp_free(arr, pin_dev_alloc);
   omp_destroy_allocator(pin_dev_alloc);
   
   return errors;
}

int main() {
   OMPVV_TEST_OFFLOADING;
   int errors = 0;
   
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_pin_device() != 0);
   
   OMPVV_REPORT_AND_RETURN(errors);
}
