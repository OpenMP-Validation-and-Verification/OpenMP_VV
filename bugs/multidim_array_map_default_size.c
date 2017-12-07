/* 
 * This code crashes cray due to the default
 * size and offset mapping of the multidimensional array
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define VAR_A 10
#define VAR_B 10

int multdim_arr[VAR_A][VAR_B];

int copyIn() {
#pragma omp target enter data map (alloc: multdim_arr[:][:])
}

int copyOut() {
#pragma omp target exit data map (delete: multdim_arr[:][:])
}

int main() {
  copyIn();
  copyOut();
  return 0;
}



