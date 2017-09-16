// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

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
    
    // Associate device pointer to host pointer
    void * d_h_matrix = omp_target_alloc(N, omp_get_default_device());
    if (omp_target_associate_ptr((void *) h_matrix[dev], d_h_matrix, N, 0, omp_get_default_device()) != 0) {
        puts("omp_target_associate_ptr fails when associating h_matrix[%d] in device %d.",dev ,omp_get_default_device());
        errors = 1;
    }
    void * dIsHost = omp_target_alloc(sizeof(int), omp_get_default_device());
    if (omp_target_associate_ptr((void *) &isHost, dIsHost, sizeof(int), 0, omp_get_default_device()) != 0) {
        puts("omp_target_associate_ptr fails when associating isHost in device %d.",omp_get_default_device());
        errors = 1;
    }

    // unstructured mapping
    {
#pragma omp target enter data map(alloc: h_matrix[dev][0 : N]) // omp_target_alloc sets ref to infinity. alloc: has effect only if ref is zero (page 217 line 21 - Version 4.5 November 2015)
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
    
    // Check if host variable is associated
    if (omp_target_is_present((void *) h_matrix[dev], omp_get_default_device()) != 0) {
        puts("h_matrix[%d] is not present in the device %d", dev, omp_get_default_device());
        errors = 1;
    }
    if (omp_target_is_present((void *) isHost, omp_get_default_device()) != 0) {
        puts("isHost is not present in the device %d", omp_get_default_device());
        errors = 1;
    }
    // No exit data. Manual copy using API
    if (omp_target_memcpy((void *) h_matrix[dev],d_h_matrix, N, 0, 0, omp_get_initial_device(), omp_get_default_device()) != 0) {
        puts("memcopy error of h_matrix[%d] from device %d to host %d",dev, omp_get_default_device(), omp_get_initial_device());
        errors = 1;
    }
    if (omp_target_memcpy((void *) isHost, dIsHost, sizeof(int), 0, 0, omp_get_initial_device(), omp_get_default_device()) != 0) {
        puts("memcpy error of isHost from device %d to host %d", omp_get_default_device(), omp_get_initial_device());
        errors = 1;
    }
    omp_target_free(d_h_matrix, omp_get_default_device());
    omp_target_free(dIsHost, omp_get_default_device());
  }

  // checking results
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
    // Associate device pointer to host pointer
    void * d_h_matrix = omp_target_alloc(N, dev);
    if (omp_target_associate_ptr((void *) h_matrix[dev], d_h_matrix, N, 0,dev) != 0) {
        puts("omp_target_associate_ptr fails when associating h_matrix[%d] in device %d.",dev ,dev);
        errors = 1;
    }
    void * dIsHost = omp_target_alloc(sizeof(int),dev);
    if (omp_target_associate_ptr((void *) &isHost, dIsHost, sizeof(int), 0,dev) != 0) {
        puts("omp_target_associate_ptr fails when associating isHost in device %d.",dev);
        errors = 1;
    }
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
    // Check if host variable is associated
    if (omp_target_is_present((void *) h_matrix[dev],dev) != 0) {
        puts("h_matrix[%d] is not present in the device %d", dev,dev);
        errors = 1;
    }
    if (omp_target_is_present((void *) isHost, dev) != 0) {
        puts("isHost is not present in the device %d", dev);
        errors = 1;
    }
    // No exit data. Manual copy using API
    if (omp_target_memcpy((void *) h_matrix[dev],d_h_matrix, N, 0, 0, omp_get_initial_device(),dev) != 0) {
        puts("memcopy error of h_matrix[%d] from device %d to host %d",dev, dev, omp_get_initial_device());
        errors = 1;
    }
    if (omp_target_memcpy((void *) isHost, dIsHost, sizeof(int), 0, 0, omp_get_initial_device(), dev) != 0) {
        puts("memcpy error of isHost from device %d to host %d", dev, omp_get_initial_device());
        errors = 1;
    }
    omp_target_free(d_h_matrix, dev);
    omp_target_free(dIsHost, dev);

  }

  // checking results
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

int test_more_devices() {

  printf("test_more_devices\n");

  // Get number of devices
  int real_num_dev = omp_get_num_devices();
  int num_dev = real_num_dev + 2;
  printf("num_devices: %d, real_num_devices: %d\n", num_dev, real_num_dev);

  int def_dev = omp_get_default_device();
  printf("initial device: %d\n", omp_get_initial_device());
  printf("default device: %d\n", def_dev);

  int sum[num_dev], errors = 0, extra_on_host = 0, isHost = 0;
  int h_matrix[num_dev][N];

  // omp_set_default_device is implementation dependent
  for (int dev = 0; dev < num_dev; ++dev) {
    omp_set_default_device(dev);
    int dev_tmp = omp_get_default_device();
    // Associate device pointer to host pointer
    void * d_h_matrix = omp_target_alloc(N, dev_tmp);
    if (omp_target_associate_ptr((void *) h_matrix[dev_tmp], d_h_matrix, N, 0,dev_tmp) != 0) {
        puts("omp_target_associate_ptr fails when associating h_matrix[%d] in device %d.",dev ,dev_tmp);
        errors = 1;
    }
    void * dIsHost = omp_target_alloc(sizeof(int),dev_tmp);
    if (omp_target_associate_ptr((void *) &isHost, dIsHost, sizeof(int), 0,dev_tmp) != 0) {
        puts("omp_target_associate_ptr fails when associating isHost in device %d.",dev_tmp);
        errors = 1;
    }
    void * dExtra_on_host = omp_target_alloc(sizeof(int),dev_tmp);
    if (omp_target_associate_ptr((void *) &extra_on_host, dExtra_on_host, sizeof(int), 0,dev_tmp) != 0) {
        puts("omp_target_associate_ptr fails when associating extra_on_host in device %d.",dev_tmp);
        errors = 1;
    }


    // unstructured mapping
    {
#pragma omp target enter data map(alloc: h_matrix[dev][0 : N])
        printf("");
    }
    // operation
    // assert(dev == dev_tmp);
#pragma omp target map(alloc: h_matrix[dev][0 : N]) map(tofrom: isHost) map(tofrom : extra_on_host)
    {
      isHost = omp_is_initial_device();
      for (int i = 0; i < N; ++i)
        h_matrix[dev][i] = dev;
      if (dev >= real_num_dev)
          extra_on_host = 1;
    }
    // Check if host variable is associated
    if (omp_target_is_present((void *) h_matrix[dev],dev_tmp) != 0) {
        puts("h_matrix[%d] is not present in the device %d", dev,dev_tmp);
        errors = 1;
    }
    if (omp_target_is_present((void *) isHost, dev_tmp) != 0) {
        puts("isHost is not present in the device %d", dev_tmp);
        errors = 1;
    }
    if (omp_target_is_present((void *) extra_on_host, dev_tmp) != 0) {
        puts("extra_on_host is not present in the device %d", dev_tmp);
        errors = 1;
    }

    // No exit data. Manual copy using API
    if (omp_target_memcpy((void *) h_matrix[dev],d_h_matrix, N, 0, 0, omp_get_initial_device(),dev_tmp) != 0) {
        puts("memcopy error of h_matrix[%d] from device %d to host %d",dev, dev_tmp, omp_get_initial_device());
        errors = 1;
    }
    if (omp_target_memcpy((void *) isHost, dIsHost, sizeof(int), 0, 0, omp_get_initial_device(), dev_tmp) != 0) {
        puts("memcpy error of isHost from device %d to host %d", dev_tmp, omp_get_initial_device());
        errors = 1;
    }
    if (omp_target_memcpy((void *) extra_on_host, dExtra_on_host, sizeof(int), 0, 0, omp_get_initial_device(), dev_tmp) != 0) {
        puts("memcpy error of extra_on_host from device %d to host %d", dev_tmp, omp_get_initial_device());
        errors = 1;
    }
    omp_target_free(d_h_matrix, dev_tmp);
    omp_target_free(dIsHost, dev_tmp);
    omp_target_free(dExtra_on_host, dev_tmp);
  }

  // checking results
  errors = 0;
  for (int dev = 0; dev < num_dev; ++dev) {
    sum[dev] = h_matrix[dev][0];
    for (int i = 1; i < N; ++i)
      sum[dev] += h_matrix[dev][i];
    errors |= (dev * N != sum[dev]);
    if (errors)
      printf("Error at dev=%d\n", dev);
  }

  if (!errors)
    printf("Test passed: num_devices = %d, real_num_dev = %d, extra_on_host = "
           "%d\n",
           num_dev, real_num_dev, extra_on_host);
  else
    printf("Test failed: num_devices = %d, real_num_dev = %d, extra_on_host = "
           "%d\n",
           num_dev, real_num_dev, extra_on_host);

  if (!errors)
    printf("Test passed on %s: num_devices = %d, real_num_dev = %d, extra_on_host = %d\n", (isHost ? "host" : "device"), num_dev, real_num_dev, extra_on_host);
  else
    printf("Test failed on %s: num_devices = %d, real_num_dev = %d, extra_on_host = %d\n", (isHost ? "host" : "device"), num_dev, real_num_dev, extra_on_host);

  omp_set_default_device(def_dev);

  return errors;
}

int main() {

  int errors = 0;

  errors += test_set_default_dev();
  errors += test_device();
  errors += test_more_devices();

  return errors;
}
