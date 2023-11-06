//------------------------------------------- test_taskloop_collapse.c --------------------------------------//
//
// OpenMP API Version 4.5 September 2015
//
// This test checks the 'taskloop' directive with the 'collapse' clause specified.
// The 'taskloop' construct parallelize loops with independent iterations by creating tasks. 
// It allows for efficient parallel execution of loop iterations by distributing them among multiple threads. 
// The 'collapse' clause ensures that multiple nested loops into a single loop.      
//----------------------------------------------------------------------------------------------------------//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define NUM_THREADS 100
#define NUM_COLLAPSE 2

int test_taskloop_collapse() {

  int errors = 0;

  int WEEK = 1, DAYS_IN_WEEK = 7, sum_with_openmp = 0, sum_without_openmp = 0;

   //get valid sum without openmp
   for(int i = 1; i <= (WEEK*DAYS_IN_WEEK); i++) {
     sum_without_openmp = sum_without_openmp + i;
   }

   #pragma omp parallel num_threads(NUM_THREADS)
   {
      #pragma omp single
      {
	#pragma omp taskloop collapse(NUM_COLLAPSE) 
        for(int i = 1; i <= WEEK; i++) 
	{
	    for(int j = 1; j <= DAYS_IN_WEEK; j++) 
	    {
		    #pragma omp atomic 
		    sum_with_openmp = sum_with_openmp + (i*j);
            }
	}
      }	   
   }
   
   OMPVV_TEST_AND_SET_VERBOSE(errors, sum_with_openmp != sum_without_openmp);

   return errors;
}

int main() {
  
  int errors = 0;
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, (test_taskloop_collapse()) != 0);
  OMPVV_REPORT_AND_RETURN(errors);

  return 0;
}
