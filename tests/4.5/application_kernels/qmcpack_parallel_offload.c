//===---- test_target_parallel.c - combined consutrct target and parallel  -------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// This test checks for multiple parallel offloading kernels. 
// Kernel1 does parallel offload without specifying a device id while 
// kernel2 tries to offload to specific devices.
// Adapted from https://reviews.llvm.org/D74145 to match V&V coding and
// reporting style. 
//
//===----------------------------------------------------------------------------------===//
//
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#include "ompvv.h"


void qmcpack_kernel1(int ndev) {

  const int N = 1024, num_teams = 8, num_th = 4;
  OMPVV_INFOMSG("qmcpack_multiple_threads_offloading");

  //Spawn 1 thread per device
#pragma omp parallel for num_threads(ndev)
  for (int i = 0; i < ndev; ++i) {
#pragma omp target teams distribute parallel for num_teams(num_teams) num_threads(num_th)
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < N/2 ; ++j) {
        int tmp_val = N *i*j;
      }
    }
  }
}
void qmcpack_kernel2(int ndev) {

  const int N = 1024, num_teams = 8, num_th = 4;
  OMPVV_INFOMSG("qmcpack_multiple_threads_offloading");

  //Spawn 1 thread per device
#pragma omp parallel for num_threads(ndev)
  for (int i = 0; i < ndev; ++i) {
#pragma omp target teams distribute parallel for device(i % ndev) num_teams(num_teams) num_threads(num_th)
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < N/2 ; ++j) {
        int tmp_val = N *i*j;
      }
    }
  }
}

int main(int argc, char *argv[]) {
  int i;
  const int N = 1000;
  int num_dev = omp_get_num_devices();
  //printf("No. of devices available %d.\n", num_dev);
  
  OMPVV_WARNING_IF(num_dev <= 1, "Multiple devices not available");

  OMPVV_TEST_OFFLOADING;

  clock_t start = clock();

  for (int i = 0; i < N; ++i) {
    qmcpack_kernel1(num_dev);
  }

  const clock_t duration1 = (clock() - start) * 1000 / CLOCKS_PER_SEC / N;
  
  if(num_dev > 1){
    start = clock();
    for (int i = 0; i < N; ++i) {
      qmcpack_kernel2(num_dev);
    }
    const clock_t duration2 = (clock() - start) * 1000 / CLOCKS_PER_SEC / N;
    printf("Avg time for parallel offloading to multiple devices: %ld ms\n",duration2);
  }
  printf("Avg time for parallel offloading: %ld ms\n",duration1);
  OMPVV_REPORT_AND_RETURN(0);

  return 0;
}


