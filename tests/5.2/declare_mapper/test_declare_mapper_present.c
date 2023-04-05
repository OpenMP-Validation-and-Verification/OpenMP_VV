//===---- test_declare_mapper_present.c -------------------------------------===//
// 
// OpenMP API Version 5.2 
//
// This example has been adapted from the 5.2 OpenMP Examples document:
// "declare mapper Directive", and "OpenMP Directive Syntax -> complex iterator"
// The declare mapper directive will be used to automatically map variables
// according to its prescription:  full structure, plus the dynamic storage of the
// data element. These variable should be present on the device.
//
//===-------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1000

typedef struct myvec{
    size_t len;
    double *data;
} myvec_t;


#pragma omp declare mapper(default:myvec_t v) map(present, tofrom: v, v.data[0:v.len]) 

void init( myvec_t *s )
{ 
  for(size_t i = 0; i < s->len; i++)
    s->data[i] = i; 
}


int test_declare_mapper_present() { 

  OMPVV_INFOMSG("test_declare_mapper_present");
  int errors = 0;

   myvec_t s;

   s.data = (double *)calloc(N,sizeof(double));
   s.len  = N;
  #pragma omp target data map(tofrom: s, s.data[0:s.len])
  {
    #pragma omp target
    {
        init(&s);
    }
  }

  for (int i = 0; i < N; ++i) {
    OMPVV_TEST_AND_SET(errors, s.data[i] != i);
  }	
  
  return errors;
}

int main () {
  OMPVV_TEST_OFFLOADING;

  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_declare_mapper_present());

  OMPVV_REPORT_AND_RETURN(errors);
}  
