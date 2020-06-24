//===--- test_target_update_mapper_to_discontiguous.c -----------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// from([mapper(mapper-identifier):]locator-list) : check for array sections
// with discontiguous storage
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

int i;
int errors;

#pragma omp declare mapper(new_id : newvec_t v) map(v, v.data[0:v.len])

int main() {
  
  OMPVV_TEST_OFFLOADING;

  newvec_t s;

  s.data = (double *)calloc(N,sizeof(double));
  s.len = N;
 
  //initalize the struct
  for (i = 0; i < s.len; i++) {
    s.data[i] = i;
  }

#pragma omp target update to (mapper(new_id))

#pragma omp target 
{
  for (i =0; i < s.len; i++) {
    if(s.data[i] != i) {
      errors++;
    }
  } 
} //end target

  
  OMPVV_REPORT_AND_RETURN(errors);
  
}
