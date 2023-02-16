//===--- test_target_update_mapper_from_discontiguous.c -----------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test seeks to ensure that target update with motion-clause "from" can properly
// map data from the device by specifying a user-defined mapper. Additionally, the test
// checks a new addition to target update in OpenMP 5.0 that states "List items in the
// to or from clauses may include array sections with stride expressions." 
//
// Adopted from OpenMP 5.0 Example target_mapper.1.c
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

typedef struct newvec {
  size_t len;
  double *data;
} newvec_t;

size_t i;
int errors = 0;

#pragma omp declare mapper(newvec_t v) map(v, v.data[0:v.len])

int target_update_from_mapper() {
  
  newvec_t s; 

  s.data = (double *)calloc(N,sizeof(double));
  s.len = N;
  for (i = 0; i< s.len; i++){ //initalize the struct
    s.data[i] = 0;
  }
  #pragma omp target
  {
    for (i = 0; i < s.len; i += 2) {
      s.data[i] = i;
    }
  }//end target
  #pragma omp target update from(s)
  for (i =0; i < s.len; i += 2) {
    OMPVV_TEST_AND_SET(errors, s.data[i] != i);
  } 
  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, target_update_from_mapper());
  OMPVV_REPORT_AND_RETURN(errors);
}
