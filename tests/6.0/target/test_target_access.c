//===--- test_target_access_single.c --------------------===//
//
// OpenMP API Version 6.0
// Tests that the target_access single allocator trait works
// as intended for basic device-host operations and multiple 
// device access.
//
//===----------------------------------------------------===//
#include <omp.h>
#include <stdio.h>
#include "ompvv.h"
#define N 16

int test_target_access_single() {
  int errors = 0;
  int *array;
  int num_devices = omp_get_num_devices();
  
  OMPVV_WARNING_IF(num_devices < 2, "Test requires at least 2 devices, but only %d found.", num_devices);
  if (num_devices < 2) {
    return errors;
  }
  
  int first_dev = 0;
  int second_dev = 1;
  
  omp_alloctrait_t trait = { omp_atk_target_access, omp_atv_single };
  omp_allocator_handle_t single_allocator = omp_init_allocator(omp_default_mem_space, 1, &trait);
  
  array = (int*)omp_alloc(N * sizeof(int), single_allocator);
  
  for (int i = 0; i < N; i++) {
    array[i] = i;
  }
  
  // Using first device
  #pragma omp target enter data map(to: array[:N]) device(first_dev)
  
  #pragma omp target is_device_ptr(array) device(first_dev)
  {
    for (int i = 0; i < N; i++) {
      array[i] = i * N;
    }
  }
  
  #pragma omp target exit data map(delete: array[:N]) device(first_dev)
  
  // Using second device
  #pragma omp target enter data map(to: array[:N]) device(second_dev)
  
  #pragma omp target is_device_ptr(array) device(second_dev)
  {
    for (int i = 0; i < N; i++) {
      array[i] += 5; 
    }
  }
  
  #pragma omp target exit data map(delete: array[:N]) device(second_dev)
  
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, array[i] != i * N + 5);
  }
  
  omp_free(array, single_allocator);
  omp_destroy_allocator(single_allocator);
  
  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  
  int errors = 0;
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_access_single() != 0);
  
  OMPVV_REPORT_AND_RETURN(errors);
}
