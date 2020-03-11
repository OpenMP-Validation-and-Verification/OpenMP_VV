#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1000

int test_set_default_dev() {

  OMPVV_INFOMSG("test_set_default_dev");

  // Get number of devices
  int num_dev = omp_get_num_devices();
  printf("num_devices: %d\n", num_dev);

  int def_dev = omp_get_default_device();
  printf("initial device: %d\n", omp_get_initial_device());
  printf("default device: %d\n", def_dev);

  int sum[num_dev], errors = 0;
  int h_matrix[num_dev][N];

  for (int dev = 0; dev < num_dev; ++dev) {
    omp_set_default_device(dev);

#pragma omp target enter data map(alloc: h_matrix[dev][0 : N])
    
    // assert(dev == omp_get_default_device() && "message");
#pragma omp target map(alloc: h_matrix[dev][0 : N]) // map(alloc: ) to avoid target to map the entire matrix h_matrix[dev][:]
    {
      for (int i = 0; i < N; ++i)
        h_matrix[dev][i] = dev;
    }
    // unstructured exit

#pragma omp target exit data map(from: h_matrix[dev][0 : N])
  }

  // checking results
  errors = 0;
  for (int dev = 0; dev < num_dev; ++dev) {
    sum[dev] = h_matrix[dev][0];
    for (int i = 1; i < N; ++i)
      sum[dev] += h_matrix[dev][i];
    OMPVV_TEST_AND_SET_VERBOSE(errors, (dev * N != sum[dev]));
  }

  omp_set_default_device(def_dev);

  return errors;
}

int test_device() {

  OMPVV_INFOMSG("test_device");

  // Get number of devices
  int num_dev = omp_get_num_devices();
  OMPVV_INFOMSG("num_devices: %d", num_dev);

  OMPVV_INFOMSG("initial device: %d", omp_get_initial_device());
  OMPVV_INFOMSG("default device: %d", omp_get_default_device());

  int sum[num_dev], errors = 0, isHost = 0;
  int h_matrix[num_dev][N];

  for (int dev = 0; dev < num_dev; ++dev) {

#pragma omp target enter data map(alloc: h_matrix[dev][0 : N]) device(dev) 

#pragma omp target map(alloc: h_matrix[dev][0 : N]) device(dev)
    {
      for (int i = 0; i < N; ++i)
        h_matrix[dev][i] = dev;
    }

#pragma omp target exit data map(from: h_matrix[dev][0 : N]) device(dev)
  
}

  // checking results
  errors = 0;
  for (int dev = 0; dev < num_dev; ++dev) {
    sum[dev] = h_matrix[dev][0];
    for (int i = 1; i < N; ++i)
      sum[dev] += h_matrix[dev][i];
    OMPVV_TEST_AND_SET_VERBOSE(errors, (dev * N != sum[dev]));
  }

  return errors;
}

int main() {

  int errors = 0;
  
  //Test for offloading
  int is_offloading;
  OMPVV_TEST_AND_SET_OFFLOADING(is_offloading);

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_set_default_dev());
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_device());

  OMPVV_REPORT_AND_RETURN(errors);
}
