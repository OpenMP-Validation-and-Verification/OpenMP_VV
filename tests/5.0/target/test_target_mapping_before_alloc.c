//===--- test_target_mapping_before_alloc.c ------------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// The description of the map clause was modified to clarify the mapping order when
// multiple map-types are specified for a variable or structure members of a variable
// on the same construct.
//
// For a given construct, the effect of a map clause with the to, from, or tofrom map-type
// is ordered before the effect of a map clause with the alloc map-type.
////===--------------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int errors;

int to_before_alloc() {

  int i;
  int scalar = 80;
  int a[N];

  struct {
  int var;
  int b[N];
  } member; 

  member.var = 1;
  
  for (i = 0; i < N; i++) { 
    a[i] = i;
    member.b[i] = i;
  }

#pragma omp target  map (alloc: scalar, a, member) map(to: scalar, a, member) 
  {
    if (scalar != 80 || a[1] != 2 || member.var != 1 || member.b[1] != 2) {
      errors++;
    }
  }	 
  return errors; 
}

int main () {
  
  int errors = 0;
  
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, to_before_alloc());
  OMPVV_REPORT_AND_RETURN(errors);
}



