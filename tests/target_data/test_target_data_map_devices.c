#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define N 1000

// Test for OpenMP 4.5 target data to multiple devices using API
int test_map_set_default_dev() {

  puts("test_map_set_default_dev");

  // Get number of devices 
  int num_dev = omp_get_num_devices();
  printf("num_devices: %d\n", num_dev);

  int def_dev = omp_get_default_device();
  printf("initial device: %d\n", omp_get_initial_device());
  printf("default device: %d\n", def_dev);

  int sum[num_dev], errors = 0, isHost = 0;
  int* h_matrix = (int*) malloc(num_dev*N*sizeof(int));

  for (int dev = 0; dev < num_dev; ++dev) {
    omp_set_default_device(dev);
#pragma omp target data map(from: h_matrix[dev*N:N]) map(tofrom: errors)
    {
      errors = dev == omp_get_default_device();
#pragma omp target map(from: h_matrix[dev*N:N]) map(tofrom: isHost)
      {
        isHost = omp_is_initial_device();
        for (int i = 0; i < N; ++i)
          h_matrix[dev*N + i] = dev;
      } // end target
    } // end target data
  }

  // checking results 
  errors = 0;
  for (int dev = 0; dev < num_dev; ++dev) {
    sum[dev] = h_matrix[dev*N + 0];
    for (int i = 1; i < N; ++i)
      sum[dev] += h_matrix[dev*N + i];
    errors |= (dev * N != sum[dev]);
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s: num_devices = %d\n", (isHost ? "host" : "device"), num_dev);

  omp_set_default_device(def_dev);

  return errors;
}

// Test for OpenMP 4.5 target data to multiple devices using directives
int test_map_device() {

  puts("test_map_device");

  // Get number of devices 
  int num_dev = omp_get_num_devices();
  printf("num_devices: %d\n", num_dev);

  printf("initial device: %d\n", omp_get_initial_device());
  printf("default device: %d\n", omp_get_default_device());

  int sum[num_dev], errors = 0, isHost = 0;
  int* h_matrix = (int*) malloc(num_dev*N*sizeof(int));

  for (int dev = 0; dev < num_dev; ++dev) {
#pragma omp target data map(from: h_matrix[dev*N:N]) device(dev)
    {
#pragma omp target map(from: h_matrix[dev*N:N]) device(dev) map(tofrom: isHost)
      {
        isHost = omp_is_initial_device();
        for (int i = 0; i < N; ++i)
          h_matrix[dev*N + i] = dev;
      } // end target
    } // end target data
  }

  // checking results 
  errors = 0;
  for (int dev = 0; dev < num_dev; ++dev) {
    sum[dev] = h_matrix[dev*N + 0];
    for (int i = 1; i < N; ++i)
      sum[dev] += h_matrix[dev*N + i];
    errors |= (dev * N != sum[dev]);
  }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s: num_devices = %d\n", (isHost ? "host" : "device"), num_dev);

  return errors;
}

// We are commenting this code out because passing more devices that available
// is not supported by the specifications. This test is passing on clang/20170629,
// xl/20170727-beta and gcc/7.1.1-20170802 but produces a side effect runtime error
// 1587-160 on xl and clang. Gcc produces no error

//// Test for OpenMP 4.5 target data to more than available devices using API
//int test_more_devices() {
//
//  puts("test_more_devices");
//
//  // Get number of devices 
//  int real_num_dev = omp_get_num_devices();
//  int num_dev = real_num_dev + 2;
//  printf("num_devices: %d, real_num_devices: %d\n", num_dev, real_num_dev);
//
//  int def_dev = omp_get_default_device();
//  printf("initial device: %d\n", omp_get_initial_device());
//  printf("default device: %d\n", def_dev);
//
//  int sum[num_dev], errors = 0, extra_on_host = 0, isHost = 0;
//  int* h_matrix = (int*) malloc(num_dev*N*sizeof(int));
//
//  // omp_set_default_device is implementation dependent 
//  for (int dev = 0; dev < num_dev; ++dev) {
//    omp_set_default_device(dev);
//    int dev_tmp = omp_get_default_device();
//#pragma omp target data map(from: h_matrix[dev_tmp*N:N])
//    {
//      errors = dev == dev_tmp;
//#pragma omp target map(from: h_matrix[dev_tmp*N:N])    \
//        map(tofrom: extra_on_host) map(tofrom: isHost)
//      {
//        isHost = omp_is_initial_device();
//        if (!isHost) {
//          for (int i = 0; i < N; ++i)
//            h_matrix[dev_tmp*N + i] = dev;
//        } else {
//          // allows implementations that map extra devices to the host 
//          extra_on_host = 1;
//          for (int i = 0; i < N; ++i)
//            h_matrix[dev_tmp*N + i] = dev;
//        }
//      } // end target
//    } // end target data
//  }
//
//  // checking results 
//  errors = 0;
//  for (int dev = 0; dev < num_dev; ++dev) {
//    sum[dev] = h_matrix[dev*N + 0];
//    for (int i = 1; i < N; ++i)
//      sum[dev] += h_matrix[dev*N + i];
//    errors |= (dev*N != sum[dev]);
//    if (errors)
//      printf("Error at dev=%d\n", dev);
//  }
//
//  if (!errors)
//    printf("Test passed on %s\n", (isHost ? "host" : "device"));
//  else
//    printf("Test failed on %s: num_devices = %d, real_num_dev = %d, extra_on_host = %d\n", (isHost ? "host" : "device"), num_dev, real_num_dev, extra_on_host);
//
//  omp_set_default_device(def_dev);
//
//  return errors;
//}

int main() {

  int errors = 0;

  errors += test_map_set_default_dev();
  errors += test_map_device();
  //errors += test_more_devices();

  return errors;
}
