//===---- test_declare_target_device_type_nohost.c  ----------------------------------===//
// 
// OpenMP API Version 5.0 
//
// The declare target directive specifies that variables, functions(C,C++ and Fortran),
// and subroutines (Fortran) are mapped to a device. If a device_type
// clause is present on the contained declare target directive, then its argument 
// determines which versions are made available. If device_type(nohost) is present 
// only a device version of the procedure is made available. 
// 
//===-------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024
int errors = 0;

#pragma omp declare target
int a[N], b[N], c[N];  
int i = 0;
#pragma omp end declare target

#pragma omp begin declare variant match(device={kind(host)})
 void update() {
   assert(false && "update() should never be called from the host!");
 }
#pragma omp end declare variant

#pragma omp begin declare variant match(device={kind(nohost)})
 void update() {
   for (i = 0; i < N; i++) {
     a[i] += 1;
     b[i] += 2;
     c[i] += 3;
   }
 }
#pragma omp end declare variant


#pragma omp declare target to(update) device_type(nohost)

int test_declare_target_device_type_nohost() { 

  #pragma omp target update to(a,b,c)
  #pragma omp target  
  {
    if (!omp_is_initial_device()){
      update();
    }
    
    for (i = 0; i < N; i++) {
      a[i] += i;
      b[i] += 2 * i;
      c[i] += 3 * i;
    }
  }
  #pragma omp target update from (a,b,c)
  
  for (i = 0; i < N; i++) { //check array values on host
    if ( a[i] != 2 * i + 1 || b[i] != 4 * i + 2 || c[i] != 6 * i + 3 ) {
      errors++;
    }
  }
  
  return errors;
}

int main () {
  
  //initalize arrays on host
  for (i = 0; i < N; i++) {
    a[i] = i;
    b[i] = 2 * i;
    c[i] = 3 * i;
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_declare_target_device_type_nohost());
  OMPVV_REPORT_AND_RETURN(errors);
}  
