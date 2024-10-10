//===------ test_target_interchange.c ----------------===//
//
// OpenMP API Version 6.0
//
// This test evaluates the interchange directive, ensuring
// proper behavior.
//
//===-------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 10
#define M 10

int test_target_interchange() {
        int errors = 0;
        int arrayDevice[N][M];
	int arrayHost[N][M];
	int a[N*M];
	int b[N*M];
	#pragma omp target map(tofrom: a)
        {
	  int counter = 0;
	  #pragma omp interchange
          for (int i = 0; i < N; i++) {
  	    for(int j = 0; j < M; j++){
	      a[counter] = i;
	      a[counter + 1] = j;
	      counter += 2;
	    }
	  }
	}
	int host_counter = 0;
	for (int j = 0; j < M; j++) {
            for(int i = 0; i < N; i++){
              b[host_counter] = i;
              b[host_counter + 1] = j;
              host_counter += 2;
	    }
	}
	for (int i = 0; i < N * M; i++){
	    OMPVV_TEST_AND_SET(errors, b[i] != a[i]);
	}
        return errors;
}

int main() {
        OMPVV_TEST_OFFLOADING;
        int errors = 0;

        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_interchange() != 0);

        OMPVV_REPORT_AND_RETURN(errors);
}
