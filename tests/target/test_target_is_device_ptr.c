#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

// Test for OpenMP 4.5 target is_device_ptr with allocation on device with omp_target_alloc
int main() {
  int errors = 0, len = 10000, isHost = -1, device_num = 0;
  int *array_device = NULL;
  int *array_host = NULL;

  device_num = omp_get_default_device();
  array_device = (int *)omp_target_alloc(len*sizeof(int),device_num);
  array_host = (int *)malloc(len*sizeof(int));

  for (int i = 0; i < len; ++i)
    array_host[i] = i;

#pragma omp target is_device_ptr(array_device) map(tofrom: array_host[0:len]) map(tofrom: isHost)
{
      isHost = omp_is_initial_device();
      if(!isHost){
        for (int i = 0; i < len; ++i) {
          array_device[i] = i;
          array_host[i] += array_device[i];
        } 
      }
      else{
        printf("Test test_target_is_device_ptr.c skipped\n");
      }
} // end target
  //The test is skipped if the allocation on default device is not possible
  //Using omp_target_alloc without offloading to device == omp_get_default_device() 
  //leads to a Segmentation fault (with clang).
  if(isHost)
    return 0;

  // checking results
  for (int i = 0; i < len; ++i) {
    if (array_host[i] != 2*i)
      errors = 1;
  }

  omp_target_free(array_device, device_num);
  free(array_host);

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}
