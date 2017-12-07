#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define VAR_A 10
#define VAR_B 10

int main() {

  int multdim_arr[VAR_A][VAR_B][VAR_A + VAR_B];

#pragma omp target enter data map (alloc: multdim_arr[:VAR_A][:VAR_B][:VAR_A+VAR_B])

  printf("");

#pragma omp target enter exit map (delete: multdim_arr[:VAR_A][:VAR_B][:VAR_A+VAR_B])

}



