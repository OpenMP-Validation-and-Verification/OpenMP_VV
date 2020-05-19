//===----- test_target_depends.c -----------------------------------------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
//
// This test checks functionality of the depend clause with various dependence types
// and map-type-modifiers. Two arrays are initalized on the host and updated within
// several target regions that all utilize the depend clause with varying specified
// dependence types. At the end, array values are verified on the host to ensure that 
// synchronization did not result in data races and values were mapped back to device.
//
////===-------------------------------------------------------------------------------===//

#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1000

int test_all_dependencies() {
  OMPVV_INFOMSG("test_all_dependencies");

  int errors = 0;
  int dep_1[N], dep_2[N];

  // Initialize dep_1 and dep_2
  for (int i = 0; i < N; ++i) {
    dep_1[i] = 0;
    dep_2[i] = 0;
  }

#pragma omp target depend(out: dep_1) map(tofrom: dep_1[0:N])
  {
    for (int i = 0; i < N; i++) {
      dep_1[i] = 1;
    }
  } // end of omp target 

#pragma omp target depend(out: dep_2) map(tofrom: dep_2[0:N])
  {
    for (int i = 0; i < N; i++) {
      dep_2[i] = 1;
    }
  } // end of omp target 

  #pragma omp task depend(inout: dep_1) depend(inout: dep_2) \
              shared(dep_1, dep_2)
  {
    for (int i = 0; i < N; ++i) {
      dep_1[i]++;
      dep_2[i]++;
    }
  }

  #pragma omp target depend(inout: dep_1) depend(inout: dep_2) \
              map(tofrom: dep_1[0:N])  map(tofrom: dep_2[0:N])
  {
    for (int i = 0; i < N; i++) {
      dep_1[i]++;
      dep_2[i]++;
    }
  } // end of omp target 

  #pragma omp target depend(in: dep_1) depend(in: dep_2) \
              map(tofrom: dep_1[0:N])  map(tofrom: dep_2[0:N])
  {
    for (int i = 0; i < N; i++) {
      dep_1[i]++;
      dep_2[i]++;
    }
  } // end of omp target 

  #pragma omp taskwait
  
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET(errors, dep_1[i] != 4);
    OMPVV_TEST_AND_SET(errors, dep_2[i] != 4);    
  }
  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_all_dependencies());

  OMPVV_REPORT_AND_RETURN(errors);
}

