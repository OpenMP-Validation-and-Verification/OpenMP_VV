/*Test target enter and exit data with allocatable arrays -- does not work with clang(Summitdev)*/
#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define N 10

void init(int **A){
  *A = (int *)malloc(N*sizeof(int));
  if(NULL == *A) exit(-1);
#pragma omp target enter data map(to: *A[:N]) 
}

void finalize(int **A){
#pragma omp target exit data map(from: *A[:N])
}

int main () {
  int isHost = 0, errors = 0;
  int *A;

  init(&A);

#pragma omp target map(tofrom: isHost) 
  {
    /*Record where the computation was executed*/
    isHost = omp_is_initial_device();
    for (int i = 0; i < N; ++i)
      A[i] = 10;
  }

  finalize(&A);

  for(int i = 0; i < N; ++i) {
    if(A[i] != 10){
      errors = 1;
      printf("A[%d]=%d\n", i, A[i]);
    }
  }

  free(A);

  if (!errors)
      printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
      printf("Test failed on %s\n", (isHost ? "host" : "device"));

  return errors;
}
