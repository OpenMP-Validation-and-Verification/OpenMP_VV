#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

// Test for OpenMP 4.5 target enter and target exit data with global arrays.

int n=10;
int *x;
int A[10]={0,0,0,0,0,0,0,0,0,0};

void init(){
 #pragma omp target enter data map(to:A[:n])
}

void finalize(){
 #pragma omp target exit data map(from:A[:n])
}

int main (){
int isHost=-1,i,errors=0;

init();

#pragma omp target map(tofrom: isHost)  
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
