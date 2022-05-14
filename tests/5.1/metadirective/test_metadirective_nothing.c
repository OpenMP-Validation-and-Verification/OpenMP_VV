//===--------------------- test_metadirective_nothing.c ---------------------===//
//
// OpenMP API Version 5.1 Nov 2020
// 
// Test for nothing directive within metadirectives. Runs a variety of
// metadirectives that check if the nothing directive is properly rendered.
// Primarily tests based on the fact that no matter what 'when' clause is 
// rendered it should result in nothing, and thus no additional pragma should
// be created. Thus, the threads should remain unchanged through this process
// and the compiler should handle it properly. Handles both device and host
// based tests.
//
////===---------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int metadirectiveOnDevice() {
   int errors = 0;
   int scalar = 0;
   int A[N];

   for (int i = 0; i < N; i++) {
      A[i] = 0;
   }

   #pragma omp metadirective \
      when( device={kind(nohost)}: nothing ) \
      when( device={arch("nvptx")}: nothing) \
      when( device={arch("amd")}: nothing ) \
      default( target map(to:from scalar, A) )
      {
         scalar = 10;
         for (int i = 0; i < N; i++) {
            A[i] = i + 2;
         }
      }

   #pragma omp metadirective \
      when( implementation={vendor(amd)}: nothing) \
      when( implementation={vendor(nvidia)}: nothing) \
      when( device={kind(nohost)}: nothing) \
      default( target map(to:from scalar, A) )
      {
         scalar = 10;
         for (int i = 0; i < N; i++) {
            A[i] = i + 2;
         }
      }

   OMPVV_INFOMSG("Test ran with a number of available devices greater than 0");
   OMPVV_INFOMSG_IF(A[0] == 0, "Test recognized device was of arch/vendor/kind nvidia, amd, or nohost");
   OMPVV_WARNING_IF(A[0] == 2 || A[0] == 4, "Test could not recognize if device was of arch/vendor/kind nvidia, amd or, nohost, even though there are devices available.");

   for (int i = 0; i < N; i++) {
	    OMPVV_TEST_AND_SET(errors, A[i] == 0 && A[i] != 2 && A[i] != 4 )
   }


  
   return 0;
}

int metadirectiveOnHost() {
  int errors = 0;
  int scalar = 0;
  int A[N];

  for (int i = 0; i < N; i++) {
     A[i] = 0;
  }

  #pragma omp metadirective \
     when( device={kind(nohost)}: nothing ) \
     when( device={arch("nvptx")}: nothing ) \
     when( device={arch("amd")}: nothing ) \
     default( parallel )
     {
        scalar = 10;
        for (int i = 0; i < N; i++) {
           A[i] = i + 2;
        }
     }


  OMPVV_WARNING_IF(A[0] == 0,"Even though no devices were available the test recognized kind/arch equal to nohost or nvptx or amd");
  

  for (int i = 0; i < N; i++) {
     OMPVV_TEST_AND_SET(A[i] == 0 && A[i] != 2) {
       errors++;
     }
  }

  return 0;
} 

int main () {
  OMPVV_TEST_OFFLOADING;

  if (omp_get_num_devices() > 0) {
    metadirectiveOnDevice();
  } else {
    metadirectiveOnHost();
  }

  OMPVV_REPORT_AND_RETURN(errors);

  return 0;
}
