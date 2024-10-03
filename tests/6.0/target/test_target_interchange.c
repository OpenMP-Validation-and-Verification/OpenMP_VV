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

	for (int i = 0; i < N; i++){
	  for (int j = 0; j < M; j++){
            arrayHost[i][j] = i * j;
	    arrayDevice[i][j] = 0;
	  }
	}
        
	#pragma omp target map(tofrom: arrayDevice)
        {
	  #pragma omp interchange
          for (int i = 0; i < N; i++) {
  	    for(int j = 0; j < M; j++){
	      arrayDevice[i][j] = i*j;
	    }
	  }
	}

        for (int i = 0; i < N; i++){
	  for(int j = 0; j < M; j++){
	    OMPVV_TEST_AND_SET(errors, arrayHost[i][j] != arrayDevice[i][j]);
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
