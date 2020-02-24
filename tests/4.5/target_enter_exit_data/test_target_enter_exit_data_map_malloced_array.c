#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

// Test for OpenMP 4.5 target enter and target exit data with allocated arrays.

int n=10;
int *x;

void init(int **A){
 *A = (int *)malloc(n*sizeof(int));
 if(NULL == *A) exit(-1);
 x=*A;
 #pragma omp target enter data map(to:x[:n])//Note:Mapping *A[:n] is incorrect as OpenMP doesn't support arbitrary expressions
}

void finalize(){
 #pragma omp target exit data map(from:x[:n])
}

int main (){
int isHost=-1,i,errors=0;
int *A;

init( &A);

#pragma omp target map(tofrom: isHost) map(to: n) 
{
 /*Record where the computation was executed*/
 isHost = omp_is_initial_device();

 for(i=0;i< n; i++)
   A[i] = 10;
}

 finalize();
 for(i=0; i<n; i++)
   if(A[i] != 10){
     errors += 1;
   }

  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}
