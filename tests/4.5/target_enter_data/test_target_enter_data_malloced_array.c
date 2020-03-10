//===---- test_target_enter_data_malloced_array.c -------------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// Tests for target enter data with heap allocated arrays. The test begins by creates
// a pointer to a space in memory that is size n*sizeof(int) and is allocated using malloc(). 
// A check is made to ensure that the pointer is not null, variable x is set equal to the 
// referenced pointer, and finally the space in memory that was allocated using malloc()
// is filled with int value 10 and global array B[10] is filled with int value 0. The values
// of x[n] are mapped onto the device using enter data map, and array B[10] is set equal to 
// values of x[n] to ensure that.... 
// 
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

// Test for OpenMP 4.5 target enter and target exit data with allocated arrays.

// global variables
int n=10, B[10];
int *x;


void init(int **A) {
 int i;
 *A = (int *) malloc(n*sizeof(int));
 if (NULL == *A) exit(-1);
 x = *A;
 for (i = 0; i < n; i++){
   x[i] = 10;
   B[i] = 0;
 }
 
#pragma omp target enter data map(to:x[:n])//Note:Mapping *A[:n] is incorrect as OpenMP doesn't support arbitrary expressions
}

int main () {

//check offloading
  int is_offloading;
  OMPVV_TEST_AND_SET_OFFLOADING(is_offloading);
  int i, errors = 0;
  int *A;

  init(&A);

#pragma omp target map(to: n) map(tofrom: B)
{
 for (i = 0; i < n; i++)
   B[i] = x[i];
}

// finalize();
 for (i = 0; i < n; i++)
   if (B[i] != 10) {
     errors += 1;
   }
 

  OMPVV_TEST_AND_SET_VERBOSE(errors, (errors != 0));

  OMPVV_REPORT_AND_RETURN(errors);
}
