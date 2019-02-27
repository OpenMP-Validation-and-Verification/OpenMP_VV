//===---- test_target_update_if.c - check the if clause of target update ------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// The if clause determines if the section should be updated on  
// the device. There are two scenarios to test here: 
// (a) with offloading when 'if' clause evaluates to true then 
// associated data is updated depending on the motion clause.
// (b) with offloading when 'if' clause evaluates to false 
// then there is no update
// The if clause is evaluated on runtime which means that variables could
// determine this behavior. 
//
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>

#define N 100
int a[N];
int b[N];
int c[N];
int count, toggle=0;

int init_b(){
  if(toggle%2){
    int i;
    for (i = 0; i < N; i++) {
      b[i] = b[i] * 2; 
    }
    toggle++;
    return 1;
  }
  else{
    toggle++;
    return 0;
  }
}
  
// Test for OpenMP 4.5 target update  with if
int main() {
  int errors[2]={0,0}, i = 0, isHost = -1, isOffloading = 0, change_flag=0;

  for (i = 0; i < N; i++) {
    a[i] = 10;
  }

  // We test for offloading
#pragma omp target map(from: isOffloading)
  {
    isOffloading = !omp_is_initial_device();
  }

for(count=0; count< 4;count++){
  // a and b array initialization
  for (i = 0; i < N; i++) {
    b[i] = 2; 
    c[i] = 0;
  }
#pragma omp target data map(to: a[:N], b[:N]) map(tofrom: c) map(tofrom: isHost)
{
  #pragma omp target //map(to: a[:N], b[:N], isHost)
  {
        isHost = omp_is_initial_device();
        int j = 0;
        for (j = 0; j < N; j++) {
          c[j] = (a[j] + b[j]);//c=12 
        }
  } // end target

  change_flag = init_b();
  #pragma omp target update if (change_flag) to(b[:N]) //update b=4 for all odd iterations

  #pragma omp target //map(from: a,b,c) map(tofrom: isHost)//mapping from to counter default tofrom mapping on b
  {
        int j = 0;
        for (j = 0; j < N; j++) {
          c[j] = (c[j] + b[j]);//if b is updated c=16 else c=14
        }
  } // end target

}// end target-data

    // checking results 
  if(change_flag){
    for (i = 0; i < N; i++) {
        if (c[i] != 16) {
          errors[0] += 1;
        }
    }
  }
  else{
    for (i = 0; i < N; i++) {
        if (c[i] != 14) {
          errors[1] += 1;
        }
    }
  }
}//end for

  if (!errors[0] && !errors[1])
    printf("Target update test passed on %s.\n", (isOffloading ? "device" : "host"));
  else if(errors[0] > 0)
    printf("Target update test when if clause is true failed on %s\n", (isOffloading ? "device" : "host"));
  else if(errors[1] > 0)
    printf("Target update test when if clause is false failed on %s\n", (isOffloading ? "device" : "host"));

  return (errors[0] + errors[1]);
}
