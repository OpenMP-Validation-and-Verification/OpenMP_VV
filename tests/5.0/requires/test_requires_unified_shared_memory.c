//===---test_requires_unified_shared_memory.c -------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
//
////===----------------------------------------------------------------------===//
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

#pragma omp requires unified_shared_memory

int unified_shared_memory_scalar() {
  OMPVV_INFOMSG("Unified shared memory testing - scalar value");
  int errors = 0;
  
  int aVariable = 0;
  // aVariable is only mapped to, with unified shared memory we should see the change reflected in the host.
  // If no map is explicitely added, then scalars are firstprivate
#pragma omp target map(to:aVariable)
  {
    aVariable += 10;
  }

  aVariable += 10;
#pragma omp target map(alloc:aVariable)
  {
    aVariable += 10;
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, aVariable != 30);
  return errors;
}

int unified_shared_memory_stack() {
  OMPVV_INFOMSG("Unified shared memory testing - Array on stack");
  int errors = 0;
  
  int anArray[N];
  int anArrayCopy[N];

  for (int i = 0; i < N; i++) {
    anArray[i] = i;
    anArrayCopy[i] = 0;
  }
  // If for any reason there is data mapping, we avoid the movement
  // that may occur through the default mapping map(tofrom)
#pragma omp target data map(alloc:anArray) 
  {
    // Modify again on the host
    for (int i = 0; i < N; i++) {
        anArray[i] += 10;
    }
#pragma omp target
    {
      for (int i = 0; i < N; i++) {
        anArray[i] += 10;
      }
    }
  }
  // Modify again on the host
  for (int i = 0; i < N; i++) {
    anArray[i] += 10;
  }
#pragma omp target map(alloc:anArray)
  {
    for (int i = 0; i < N; i++) {
      anArrayCopy[i] = anArray[i];
    }
  }
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, anArray[i] != i + 30);
    OMPVV_TEST_AND_SET_VERBOSE(errors, anArrayCopy[i] != i + 30);
  }
  return errors;
}

int unified_shared_memory_heap() {
  OMPVV_INFOMSG("Unified shared memory testing - Array on heap");
  int errors = 0;
  
  int *anArray;
  int *anArrayCopy;

  anArray = (int*)malloc(sizeof(int)*N);
  anArrayCopy = (int*)malloc(sizeof(int)*N);

  for (int i = 0; i < N; i++) {
    anArray[i] = i;
    anArrayCopy[i] = 0;
  }
  // Modiify in the device
#pragma omp target
  {
    for (int i = 0; i < N; i++) {
      anArray[i] += 10;
    }
  }
  // Modify again on the host
  for (int i = 0; i < N; i++) {
    anArray[i] += 10;
  }

  // Get the value the device is seeing
#pragma omp target 
  {
    for (int i = 0; i < N; i++) {
      anArrayCopy[i] = anArray[i];
    }
  }

  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, anArray[i] != i + 30);
    OMPVV_TEST_AND_SET_VERBOSE(errors, anArrayCopy[i] != i + 30);
  }
  return errors;
}
int main() {
  int isOffloading;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  OMPVV_WARNING_IF(!isOffloading, "With no offloading, unified shared memory is guaranteed due to host execution");
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, unified_shared_memory_scalar());
  OMPVV_TEST_AND_SET_VERBOSE(errors, unified_shared_memory_stack());
  OMPVV_TEST_AND_SET_VERBOSE(errors, unified_shared_memory_heap());

  OMPVV_REPORT_AND_RETURN(errors);
}
