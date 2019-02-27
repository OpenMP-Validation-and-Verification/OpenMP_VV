#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

// Test for OpenMP 4.5 target data with use_device_ptr
int main() {
  int errors = 0, len = 10000, isHost = 0;
  int *array_device = NULL;
  int *array_host = NULL;

  array_device = (int *)malloc(len*sizeof(int));
  array_host = (int *)malloc(len*sizeof(int));

  for (int i = 0; i < len; ++i)
    array_host[i] = i;

#pragma omp target data map(tofrom: array_device[0:len])   \
    use_device_ptr(array_device)
  {
#pragma omp target is_device_ptr(array_device) map(tofrom: array_host[0:len]) map(tofrom: isHost)
    {
      isHost = omp_is_initial_device();
      for (int i = 0; i < len; ++i) {
        array_device[i] = i;
        array_host[i] += array_device[i];
      } 
    } // end target
  } // end target data

  // checking results
  for (int i = 0; i < len; ++i) {
    if (array_host[i] != 2*i)
      errors = 1;
  }

  free(array_device);
  free(array_host);

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}
