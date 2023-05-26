//===--- test_declare_target_map_device_pointer.c ------------------------------------------===//
// 
// OpenMP API Version 5.2 Nov 2021 
//
//This test checks if, after offloading, the initalized pointer variable retains its 
//original value as per the semantics of the firstprivate clause. 
//
//===---------------------------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define N 1000

int main() {
  int compute_array[N];
  int *p;
  int sum = 0, result = 0, errors = 0;
  int i;
  
  
  // Array initialization
  for (i = 0; i < N; i++)
    compute_array[i] = i;
  p = &compute_array[0];

  int isOffloading;

  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);

  OMPVV_WARNING_IF(!isOffloading, "This test is running on host, the value of p[] is not copied over to the device"); 

  //#pragma omp target data map(tofrom: compute_array) //To test default pointer behavior, array must be mapped before the pointer
  #pragma omp target map(tofrom: p[:N]) map(tofrom: sum)
  {
    // Array modified through the pointer
    for (i = 0; i < N; i++){
      sum += p[i];
    }
   
  } // end target

  // Result comparison
  for (i = 0; i < N; i++)
    if (compute_array[i] != i)  errors++;
  
  for (i = 0; i < N; i++)
    result += i;

  OMPVV_TEST_AND_SET_VERBOSE(errors, result != sum);

  OMPVV_REPORT_AND_RETURN(errors);

}
