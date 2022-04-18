//===--- test_target_update_mapper_to_discontiguous.c -----------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test seeks to ensure that target update with motion-clause "to" can properly
// map data to the device by specifying a user-defined mapper. Additionally, the test
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
int errors;

#pragma omp declare mapper(newvec_t v) map(v, v.data[0:v.len])

int target_update_to_mapper() {
  
  OMPVV_TEST_OFFLOADING;

  newvec_t s;

  s.data = (double *)calloc(N,sizeof(double));
  s.len = N;
 
  //initalize the struct
  for (i = 0; i < s.len; i++) {
    s.data[i] = i;
  }

#pragma omp target update to(s)

#pragma omp target map(tofrom: errors)
{
  for (i =0; i < s.len; i++) {
    if(s.data[i] != i) {
      errors++;
    }
  } 
} //end target

  return errors;
  
}

int main() {
  
  OMPVV_TEST_OFFLOADING;
  
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, target_update_to_mapper());

  OMPVV_REPORT_AND_RETURN(errors);

}
