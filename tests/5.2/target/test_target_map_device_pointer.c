//===--- test_declare_target_map_device_pointer.c ------------------------------------------===//
// 
// OpenMP API Version 5.2 Nov 2021 
//
//This test checks if, after offloading, the initalized pointer variable retains its 
//original value as per the semantics of the firstprivate clause. 
//The pointer variable, nor the array it refers to, are not mapped to the device. This 
//ensures that a matching mapped data item is not found for the pointer, so it retains
//its original value.
//===---------------------------------------------------------------------------------------===//

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

#define N 1000

int test_pointer() {
  int compute_array[N];
  int *p;
  int errors = 0;
  int i;
  
  // Array initialization
  for (i = 0; i < N; i++)
    compute_array[i] = i;
  p = &compute_array[0];

  #pragma omp target defaultmap(none:pointer)
  {
    // Array modified through the pointer
    int index=0;
    for (i = N-1; i >= 0; i--){
      p[index] = i;
      index++;
    }
  } // end target

  // Pointer values comparison
  for (i = 0; i < N; i++)
    if (p[i] != i) errors++;

  OMPVV_ERROR_IF(errors!=0, "ERROR COUNT (DIFFERING VALUES): %d",errors);
  
  return errors;
}
int main(){
  int errors=0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_pointer() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
