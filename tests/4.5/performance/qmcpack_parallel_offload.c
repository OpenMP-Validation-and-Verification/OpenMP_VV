//===---- qmcpack_parallel_offload.c ---------------------------------------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// This test checks for multiple parallel offloading kernels. 
// Kernel1 does parallel offload without specifying a device number. 
// Kernel2 tries to offload to specific devices.
// Kernel3 does parallel offload with nowait without specifying a device number. 
// Kernel4 tries to offload to specific devices with nowait.
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

const int N = 1024, num_teams = 8, num_th = 4;

void qmcpack_kernel1(int ndev) {
  OMPVV_INFOMSG("qmcpack parallel offloading");

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
  OMPVV_INFOMSG("qmcpack parrallel offloading to different devices");

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

void qmcpack_kernel3(int ndev) {
  OMPVV_INFOMSG("qmcpack parallel offloading with nowait");

  //Spawn 1 thread per device
#pragma omp parallel for num_threads(ndev)
  for (int i = 0; i < ndev; ++i) {
#pragma omp target teams distribute parallel for num_teams(num_teams) num_threads(num_th) nowait
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < N/2 ; ++j) {
        int tmp_val = N *i*j;
      }
    }
  }
}

void qmcpack_kernel4(int ndev) {
  OMPVV_INFOMSG("qmcpack parrallel offloading to different devices with nowait");

  //Spawn 1 thread per device
#pragma omp parallel for num_threads(ndev)
  for (int i = 0; i < ndev; ++i) {
#pragma omp target teams distribute parallel for device(i % ndev) num_teams(num_teams) num_threads(num_th) nowait
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
  
  OMPVV_WARNING_IF(num_dev <= 1, "Multiple devices not available");

  OMPVV_TEST_OFFLOADING;

  //Kernel1 
  clock_t start = clock();
  for (int i = 0; i < N; ++i) {
    qmcpack_kernel1(num_dev);
  }
  const clock_t duration1 = (clock() - start) * 1000 / CLOCKS_PER_SEC / N;
  printf("Avg time for parallel offloading: %ld ms\n",duration1);
  
  //Kernel3
  start = clock();
  for (int i = 0; i < N; ++i) {
    qmcpack_kernel3(num_dev);
  }
  const clock_t duration3 = (clock() - start) * 1000 / CLOCKS_PER_SEC / N;
  printf("Avg time for parallel offloading with nowait: %ld ms\n",duration3);

  
  if(num_dev > 1){
    //Kernel2
    start = clock();
    for (int i = 0; i < N; ++i) {
      qmcpack_kernel2(num_dev);
    }
    const clock_t duration2 = (clock() - start) * 1000 / CLOCKS_PER_SEC / N;
    printf("Avg time for parallel offloading to multiple devices: %ld ms\n",duration2);

    //Kernel4 
    start = clock();
    for (int i = 0; i < N; ++i) {
    qmcpack_kernel4(num_dev);
    }
    const clock_t duration4 = (clock() - start) * 1000 / CLOCKS_PER_SEC / N;
    printf("Avg time for parallel offloading to multiple devices with nowait: %ld ms\n",duration4);
  }


  OMPVV_REPORT_AND_RETURN(0);

  return 0;
}


