//===--- regression_distribute_for_collapse_N_TEAMS_N_THREADS.c ----------------------------------------===//
//
// OpenMP API Version 4.5
//
// Test #pragma omp target teams distribute parallel for collapse() on an 
// N_DIM*N_DIM*N_DIM*N_DIM array by writing to it. Testing on a gloabal 
// array with at least 3-4 dimensions in order to do a for sollapse of
// at least 3-4. To ensure that each write to array occurred only once.
//
////===----------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>

#ifndef DIM_SIZE
#define DIM_SIZE 4
#endif

#ifndef N_THREADS
#define N_THREADS 128
#endif

#ifndef N_TEAMS
#define N_TEAMS 128
#endif

#ifndef COLLAPSE
#define COLLAPSE 4
#endif

int calc_index(int dim_indices, int i, int j, int k, int l){
  return dim_indices*dim_indices*dim_indices * i + dim_indices*dim_indices * j + dim_indices * k + l;
}

int test_collapse() {
  int errors = 0;
  int* teams = (int*)malloc(sizeof(int)*DIM_SIZE*DIM_SIZE*DIM_SIZE*DIM_SIZE);
  int* threads = (int*)malloc(sizeof(int)*DIM_SIZE*DIM_SIZE*DIM_SIZE*DIM_SIZE);
  int i,j,k,l;

  if (teams == NULL || threads == NULL){
    printf("malloc failed!\n");
    return 1;
  }

  for (i = 0; i < DIM_SIZE*DIM_SIZE*DIM_SIZE*DIM_SIZE; ++i) {
    teams[i] = -1;
    threads[i] = -1;
  }
#pragma omp target teams distribute parallel for collapse(COLLAPSE) \
  map(tofrom: teams[:DIM_SIZE*DIM_SIZE*DIM_SIZE*DIM_SIZE], \
              threads[:DIM_SIZE*DIM_SIZE*DIM_SIZE*DIM_SIZE]) private(i,j,k,l) \
  num_teams(N_TEAMS) num_threads(N_THREADS)

  for (i = 0; i < DIM_SIZE; ++i) {
    for (j = 0; j < DIM_SIZE; ++j) {
      for (k = 0; k < DIM_SIZE; ++k) {
        for (l = 0; l < DIM_SIZE; ++l) {
          int index = calc_index(DIM_SIZE, i, j, k, l);
          if(teams[index] == -1)
            teams[index] = omp_get_team_num();
          else
            teams[index] = -2;
          threads[index] = omp_get_thread_num();
        }
      }
    }
  }

  for (i = DIM_SIZE-1; i < DIM_SIZE; ++i) {
    for (j = DIM_SIZE-1; j < DIM_SIZE; ++j) {
      for (k = DIM_SIZE-1; k < DIM_SIZE; ++k) {
        for (l = DIM_SIZE-1; l < DIM_SIZE; ++l) {
          int index = calc_index(DIM_SIZE, i, j, k, l);
          printf("%d, %d, %d, %d, %d, %d, %d\n",i,j,k,l,index,teams[index],threads[index]); 
        }
      }
    }
  }

  for (i = 0; i < DIM_SIZE*DIM_SIZE*DIM_SIZE*DIM_SIZE; ++i) {
    if (teams[i] == -2) {
      ++errors;
      printf("MULTIPLE WRITES AT INDEX %d FOR teams ARRAY\n", i);
    }
  }

  free(teams);
  free(threads);
  return errors;
}


int main() {
  int errors = 0;
  errors = test_collapse();
  return errors;
}
