//===---- test_target_update_to.c - check the to data motion clause of target update -===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>

#define N 100
int a[N];
int b[N];
int c[N];

void update_b(){
    int i;
    for (i = 0; i < N; i++) {
      b[i] = b[i] * 2; 
    }
}
  
// Test for OpenMP 4.5 target update with to
int main() {
  int errors= 0, i = 0, isHost = -1, isOffloading = 0, change_flag=0;

  for (i = 0; i < N; i++) {
    a[i] = 10;
    b[i] = 2; 
  }

  // We test for offloading
#pragma omp target map(from: isOffloading)
  {
    isOffloading = !omp_is_initial_device();
  }

#pragma omp target data map(to: a[:N], b[:N]) map(tofrom: isHost)
{
  #pragma omp target
  {
        isHost = omp_is_initial_device();
        int j = 0;
        for (j = 0; j < N; j++) {
          b[j] = (a[j] + b[j]);//b=12 
        }
  } // end target

  #pragma omp target update from(b[:N]) //update b=12 on host 

  #pragma omp target 
  {
        int j = 0;
        for (j = 0; j < N; j++) {
          c[j] = (2* b[j]);// c=24 
        }
  } // end target

}// end target-data

    // checking results 
    for (i = 0; i < N; i++) {
        if (c[i] != 24) {
          errors += 1;
        }
    }

  if (!errors)
    printf("Test passed on %s.\n", (isOffloading ? "device" : "host"));
  else
    printf("Test failed on %s\n", (isOffloading ? "device" : "host"));

  return (errors);
}
