#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

// Test for OpenMP 4.5 target enter data with global arrays.

int n=10;
int *x;
int A[10]={10,10,10,10,10,10,10,10,10,10},B[10];


int main (){
int isHost=-1,i,errors=0;

#pragma omp target enter data map(to:A[:n])
#pragma omp target map(tofrom: isHost) map(from:B[:n])
{
 /*Record where the computation was executed*/
 isHost = omp_is_initial_device();

 for(i=0;i< n; i++)
   B[i] = A[i];
}

 for(i=0; i<n; i++)
   if(B[i] != 10){
     errors += 1;
   }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}
