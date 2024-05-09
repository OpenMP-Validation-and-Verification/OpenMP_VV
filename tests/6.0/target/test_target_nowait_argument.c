//===------ test_target_nowait_argument.c ------------------------------------===//
//
// OpenMP API Version 6.0 
//
// Tests the target directive with the nowait clause with the do_not_synchronize 
// argument.
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#pragma omp begin declare target
void update(int* num) {
 
    *num = (*num) * 3;
}
#pragma omp end declare target

int test_target_nowait_argument(int is_deferred) { 

  int errors = 0;
  int x = 2, y = 3 ;


  #pragma omp target nowait(is_deferred) map(tofrom: x)
  {
    update(&x); 
  }

  if(is_deferred){
    update(&y);
  } else {
    update(&x);
  }
  

  if(is_deferred){
    if( y != 9){
      errors++;
    }
    #pragma omp taskwait //needed to finish deferred task before return from function
    if( x != 6){
      errors++;
    }
  } else {
    if( x != 18){
      errors++;
    }
  }

  return errors;
}


int main(int argc, char*argv[]){

  OMPVV_TEST_OFFLOADING;

  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_nowait_argument(0) != 0);
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_nowait_argument(1) != 0);

  OMPVV_REPORT_AND_RETURN(errors);

}

