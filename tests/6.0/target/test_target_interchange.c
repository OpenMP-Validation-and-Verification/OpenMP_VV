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
#define M 15

int test_target_interchange() {
        int errors = 0;
	int a[N][M];
	#pragma omp target map(tofrom: a)
        {
	  int counter = 0;
	  #pragma omp interchange
          for (int i = 0; i < N; i++) {
  	    for(int j = 0; j < M; j++){
	      a[i][j] = counter++;
	    }
	  }
	}
	int host_counter = 0;
	for (int j = 0; j < M; j++) {
            for(int i = 0; i < N; i++){
	        OMPVV_TEST_AND_SET(errors, a[i][j] != host_counter++);
	    }
	}
        return errors;
}

int main() {
        OMPVV_TEST_OFFLOADING;
        int errors = 0;

        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_interchange() != 0);

        OMPVV_REPORT_AND_RETURN(errors);
}
