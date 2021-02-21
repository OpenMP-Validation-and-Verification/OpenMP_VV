//===--- test_target_in_reduction.c ------------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks task reductions for a target task resulting from a target
// construct with the 'in_reduction' clause.
//
////===-------------------------------------------------------------------------------------===//

#include <stdlib.h>
#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define N 1028

void compute_on_device(int *);
void compute_on_host(int *);

#pragma omp declare target to (compute_on_device)

int main ()
{
   int i;
   int sum = 0, total = 0, errors = 0;
   
   #pragma omp parallel master
   #pragma omp taskgroup task_reduction(+:sum)
   {
      #pragma omp target in_reduction(+:sum)
         compute_on_device(&sum);
     
      #pragma omp task in_reduction(+:sum)
         compute_on_host(&sum);
   }
   
   for (i = 0; i < N; i++) {
      total += 2;
   }
  
   if (sum != total) {
         errors++;
   }

   OMPVV_ERROR_IF(sum != total, "Target task did not participate in the reduction"); 

   OMPVV_REPORT_AND_RETURN(errors);
}

void compute_on_device(int *sum) 
{
   int i;
   for (i = 0; i < N; i++) {
      *sum += 1; 
   }
}

void compute_on_host(int *sum) 
{
   int i;
   for (i = 0; i < N; i++) {
      *sum += 1; 
   }
}
