#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define N 1000

int test_set_default_dev() {

  printf("test_set_default_dev\n");

  // Get number of devices
  int num_dev = omp_get_num_devices();
  printf("num_devices: %d\n", num_dev);

  int def_dev = omp_get_default_device();
  printf("initial device: %d\n", omp_get_initial_device());
  printf("default device: %d\n", def_dev);

  int sum[num_dev], errors = 0, isHost = 0;
  int h_matrix[num_dev][N];

  for (int dev = 0; dev < num_dev; ++dev) {
    omp_set_default_device(dev);
    // unstructured mapping
    {
#pragma omp target enter data map(alloc: h_matrix[dev][0 : N])
        printf(""); // forcing the compiler to not moving out of the scope
    }
    // operation
    // assert(dev == omp_get_default_device() && "message");
#pragma omp target map(alloc: h_matrix[dev][0 : N]) map(tofrom: isHost) // map(alloc: ) to avoid target to map the entire matrix h_matrix[dev][:]
    {
      isHost = omp_is_initial_device();
      for (int i = 0; i < N; ++i)
        h_matrix[dev][i] = dev;
    }
    // unstructured exit
    {
#pragma omp target exit data map(from: h_matrix[dev][0 : N])
        printf("");
    }
  }

  // checking results
  errors = 0;
  for (int dev = 0; dev < num_dev; ++dev) {
    sum[dev] = h_matrix[dev][0];
    for (int i = 1; i < N; ++i)
      sum[dev] += h_matrix[dev][i];
    errors |= (dev * N != sum[dev]);
  }

  if (!errors)
    printf("Test passed on %s: num_devices = %d\n", (isHost ? "host" : "device"), num_dev);
  else
    printf("Test failed on %s: num_devices = %d\n", (isHost ? "host" : "device"), num_dev);

  omp_set_default_device(def_dev);

  return errors;
}

int test_device() {

  printf("test_device\n");

  // Get number of devices
  int num_dev = omp_get_num_devices();
  printf("num_devices: %d\n", num_dev);

  printf("initial device: %d\n", omp_get_initial_device());
  printf("default device: %d\n", omp_get_default_device());

  int sum[num_dev], errors = 0, isHost = 0;
  int h_matrix[num_dev][N];

  for (int dev = 0; dev < num_dev; ++dev) {
    // unstructured mapping
    {
#pragma omp target enter data map(alloc: h_matrix[dev][0 : N]) device(dev) 
        printf("");
    }
    // operation
#pragma omp target map(alloc: h_matrix[dev][0 : N]) map(tofrom: isHost) device(dev)
    {
      isHost = omp_is_initial_device();
      for (int i = 0; i < N; ++i)
        h_matrix[dev][i] = dev;
    }
    // unstructured exit
    {
#pragma omp target exit data map(from: h_matrix[dev][0 : N]) device(dev)
        printf("");
    }
  }

  // checking results
  errors = 0;
  for (int dev = 0; dev < num_dev; ++dev) {
    sum[dev] = h_matrix[dev][0];
    for (int i = 1; i < N; ++i)
      sum[dev] += h_matrix[dev][i];
    errors |= (dev * N != sum[dev]);
  }

  if (!errors)
    printf("Test passed on %s: num_devices = %d\n", (isHost ? "host" : "device"), num_dev);
  else
    printf("Test failed on %s: num_devices = %d\n", (isHost ? "host" : "device"), num_dev);


  return errors;
}


// We are commenting this code out because passing more devices that available
// is not supported by the specifications. This test is passing on clang/20170629,
// xl/20170727-beta and gcc/7.1.1-20170802 but produces a side effect runtime error
// 1587-160 on xl and clang. Gcc produces no error

//int test_more_devices() {
//
//  printf("test_more_devices\n");
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
//  int h_matrix[num_dev][N];
//
//  // omp_set_default_device is implementation dependent
//  for (int dev = 0; dev < num_dev; ++dev) {
//    omp_set_default_device(dev);
//    int dev_tmp = omp_get_default_device();
//    // unstructured mapping
//    {
//#pragma omp target enter data map(alloc: h_matrix[dev][0 : N])
//        printf("");
//    }
//    // operation
//    // assert(dev == dev_tmp);
//#pragma omp target map(alloc: h_matrix[dev][0 : N]) map(tofrom: isHost) map(tofrom : extra_on_host)
//    {
//      isHost = omp_is_initial_device();
//      for (int i = 0; i < N; ++i)
//        h_matrix[dev][i] = dev;
//      if (dev >= real_num_dev)
//          extra_on_host = 1;
//    }
//    // unstructured exit
//    {
//#pragma omp target exit data map(from: h_matrix[dev][0 : N])
//        printf("");
//    }
//  }
//
//  // checking results
//  errors = 0;
//  for (int dev = 0; dev < num_dev; ++dev) {
//    sum[dev] = h_matrix[dev][0];
//    for (int i = 1; i < N; ++i)
//      sum[dev] += h_matrix[dev][i];
//    errors |= (dev * N != sum[dev]);
//    if (errors)
//      printf("Error at dev=%d\n", dev);
//  }
//
//  if (!errors)
//    printf("Test passed: num_devices = %d, real_num_dev = %d, extra_on_host = "
//           "%d\n",
//           num_dev, real_num_dev, extra_on_host);
//  else
//    printf("Test failed: num_devices = %d, real_num_dev = %d, extra_on_host = "
//           "%d\n",
//           num_dev, real_num_dev, extra_on_host);
//
//  if (!errors)
//    printf("Test passed on %s: num_devices = %d, real_num_dev = %d, extra_on_host = %d\n", (isHost ? "host" : "device"), num_dev, real_num_dev, extra_on_host);
//  else
//    printf("Test failed on %s: num_devices = %d, real_num_dev = %d, extra_on_host = %d\n", (isHost ? "host" : "device"), num_dev, real_num_dev, extra_on_host);
//
//  omp_set_default_device(def_dev);
//
//  return errors;
//}

int main() {

  int errors = 0;

  errors += test_set_default_dev();
  errors += test_device();
  //errors += test_more_devices();

  return errors;
}
